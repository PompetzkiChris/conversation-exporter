#lang racket/base
;; cdp.rkt — Chrome DevTools Protocol session: id-correlated calls, event waiters,
;; Runtime.evaluate helper, navigation, target creation/closing via the HTTP endpoint.
(require racket/async-channel
         racket/string
         json
         "http.rkt"
         "ws.rkt"
         "util.rkt")

(provide cdp-new-target
         cdp-close-target
         cdp-new-window-target
         cdp-list-targets
         cdp-connect
         cdp-close
         cdp-call
         cdp-expect
         cdp-wait
         cdp-unexpect
         cdp-eval
         cdp-run-async-function
         cdp-navigate
         cdp-transport-stats
         cdp-dead)

;; ---------------------------------------------------------------- HTTP target management
;; PUT /json/new?<url> -> target jsexpr (id, webSocketDebuggerUrl, ...)
(define (cdp-new-target host port [url "about:blank"])
  (http-put-json host port (string-append "/json/new?" url)))

;; A page in its own browser window.  A tab behind another tab of the same window is hidden and stops
;; rendering (no requestAnimationFrame, no IntersectionObserver), so a page that loads older content when
;; scrolled to the top never loads it there; a separate window keeps rendering (Chrome runs with
;; --disable-backgrounding-occluded-windows).  -> the /json/list entry of the new page.
(define (cdp-new-window-target host port [url "about:blank"])
  (define v (http-get-json host port "/json/version"))
  (define bc (cdp-connect (hash-ref v 'webSocketDebuggerUrl)))
  (define id
    (dynamic-wind void
                  (lambda () (hash-ref (cdp-call bc "Target.createTarget" (hasheq 'url url 'newWindow #t)) 'targetId))
                  (lambda () (cdp-close bc))))
  (let loop ([k 0])
    (define t (for/first ([x (in-list (cdp-list-targets host port))] #:when (equal? (hash-ref x 'id #f) id)) x))
    (cond [t t]
          [(< k 40) (sleep 0.05) (loop (add1 k))]
          [else (error 'cdp-new-window-target "new target ~a is not listed by /json/list" id)])))

(define (cdp-close-target host port id)
  (define-values (code headers body)
    (http-request host port "GET" (string-append "/json/close/" id)))
  (values code (bytes->string/utf-8 body #\?)))

(define (cdp-list-targets host port)
  (http-get-json host port "/json/list"))

;; ---------------------------------------------------------------- session
(struct cdp (ws
             [next-id #:mutable]
             pending          ; id -> async-channel
             waiters          ; box of (list (cons method async-channel))
             lock
             [reader #:mutable]
             [dead #:mutable]))   ; #f or reason string

(define (cdp-connect ws-url)
  (define w (ws-connect ws-url))
  (define c (cdp w 1 (make-hash) (box '()) (make-semaphore 1) #f #f))
  (set-cdp-reader! c (thread (lambda () (reader-loop c))))
  c)

(define (with-lock c thunk) (call-with-semaphore (cdp-lock c) thunk))

(define (fail-all-pending c)
  (with-lock c (lambda ()
                 (for ([(id ch) (in-hash (cdp-pending c))])
                   (async-channel-put ch 'dead))
                 (hash-clear! (cdp-pending c)))))

(define (reader-loop c)
  (with-handlers ([exn:fail? (lambda (e)
                               (unless (cdp-dead c) (set-cdp-dead! c (exn-message e)))
                               (fail-all-pending c))])
    (let loop ()
      (define-values (type payload) (ws-recv (cdp-ws c)))
      (cond
        [(eq? type 'close)
         (set-cdp-dead! c "websocket closed by browser")
         (fail-all-pending c)]
        [else
         (define msg (bytes->jsexpr payload))
         (dispatch c msg)
         (loop)]))))

(define (dispatch c msg)
  (cond
    [(and (hash? msg) (hash-has-key? msg 'id))
     (define id (hash-ref msg 'id))
     (define ch (with-lock c (lambda ()
                               (begin0 (hash-ref (cdp-pending c) id #f)
                                       (hash-remove! (cdp-pending c) id)))))
     (when ch (async-channel-put ch msg))]
    [(and (hash? msg) (hash-has-key? msg 'method))
     (define method (hash-ref msg 'method))
     (define ws (unbox (cdp-waiters c)))
     (for ([wt (in-list ws)])
       (when (equal? (car wt) method)
         (async-channel-put (cdr wt) (hash-ref msg 'params (hasheq)))))]
    [else (void)]))

;; WebSocket frame statistics (evidence for 16/64-bit lengths and continuation frames).
(define (cdp-transport-stats c) (ws-stats (cdp-ws c)))

(define (cdp-close c)
  (unless (cdp-dead c) (set-cdp-dead! c "closed by client"))
  (ws-close (cdp-ws c))
  (fail-all-pending c))

;; Send a command and wait for its reply. Returns the `result` jsexpr; raises on
;; protocol error, timeout, or dead connection.
(define (cdp-call c method [params (hasheq)] #:timeout [timeout 60])
  (when (cdp-dead c) (error 'cdp "~a: connection is dead (~a)" method (cdp-dead c)))
  (define ch (make-async-channel))
  (define id (with-lock c (lambda ()
                            (define id (cdp-next-id c))
                            (set-cdp-next-id! c (add1 id))
                            (hash-set! (cdp-pending c) id ch)
                            id)))
  (log-verbose "-> ~a ~a" id method)
  (ws-send-text (cdp-ws c) (jsexpr->string (hasheq 'id id 'method method 'params params)))
  (define msg (sync/timeout timeout ch))
  (cond
    [(not msg)
     (with-lock c (lambda () (hash-remove! (cdp-pending c) id)))
     (error 'cdp "~a: no reply within ~a s" method timeout)]
    [(eq? msg 'dead)
     (error 'cdp "~a: connection died (~a)" method (cdp-dead c))]
    [(hash-has-key? msg 'error)
     (error 'cdp "~a failed: ~a" method (jsexpr->string (hash-ref msg 'error)))]
    [else
     (log-verbose "<- ~a ~a" id method)
     (hash-ref msg 'result (hasheq))]))

;; Register interest in an event BEFORE triggering it; returns a waiter.
(define (cdp-expect c method)
  (define ch (make-async-channel))
  (define wt (cons method ch))
  (with-lock c (lambda () (set-box! (cdp-waiters c) (cons wt (unbox (cdp-waiters c))))))
  wt)

(define (cdp-unexpect c wt)
  (with-lock c (lambda () (set-box! (cdp-waiters c) (remq wt (unbox (cdp-waiters c)))))))

;; Wait for the event; returns its params or #f on timeout.  Removes the waiter.
(define (cdp-wait c wt timeout)
  (define r (sync/timeout timeout (cdr wt)))
  (cdp-unexpect c wt)
  r)

(define (describe-exception ed)
  (define ex (hash-ref ed 'exception (hasheq)))
  (define desc (if (hash? ex) (hash-ref ex 'description #f) #f))
  (define text (hash-ref ed 'text ""))
  (define line (hash-ref ed 'lineNumber #f))
  (format "~a~a~a" text
          (if desc (string-append " " desc) "")
          (if line (format " (line ~a)" line) "")))

;; Runtime.evaluate with awaitPromise/returnByValue; returns the JS value as a jsexpr
;; ('null for undefined).  Raises with the exception description on a JS exception.
(define (cdp-eval c expr #:timeout [timeout 60] #:await [await? #t] #:by-value [by-value #t])
  (define r (cdp-call c "Runtime.evaluate"
                      (hasheq 'expression expr
                              'awaitPromise await?
                              'returnByValue by-value
                              'userGesture #t)
                      #:timeout timeout))
  (when (hash-has-key? r 'exceptionDetails)
    (error 'cdp-eval "JavaScript exception: ~a" (describe-exception (hash-ref r 'exceptionDetails))))
  (define res (hash-ref r 'result (hasheq)))
  (hash-ref res 'value 'null))

;; EXTENSION POINT for the DOM lane: run an embedded async function expression
;; `(async (opts) => {...})` with `opts` (a jsexpr) and a long timeout; returns the
;; JSON-serializable result as a jsexpr.
(define (cdp-run-async-function c fn-source opts #:timeout [timeout 900])
  (define expr (string-append "(" fn-source ")(" (jsexpr->string opts) ")"))
  (cdp-eval c expr #:timeout timeout #:await #t #:by-value #t))

;; Page.navigate and wait for Page.loadEventFired (Page.enable must be on).
;; Returns #t when the load event arrived within load-timeout seconds.
(define (cdp-navigate c url #:load-timeout [load-timeout 60])
  (define wt (cdp-expect c "Page.loadEventFired"))
  (define r (cdp-call c "Page.navigate" (hasheq 'url url) #:timeout 60))
  (when (hash-has-key? r 'errorText)
    (cdp-unexpect c wt)
    (error 'cdp-navigate "Page.navigate ~a failed: ~a" url (hash-ref r 'errorText)))
  (and (cdp-wait c wt load-timeout) #t))
