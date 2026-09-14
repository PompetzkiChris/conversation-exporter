#lang racket/base
;; gui-run.rkt — off-UI-thread driver for grok-export-gui.
;;
;; It runs the EXISTING, verified exporter engine (grok-export.rkt `main`) in-process, in a
;; thread owned by its own custodian, with `current-output-port` redirected into a line-splitting
;; port and `report-hook` installed so the engine's progress events reach the GUI.  Nothing of the
;; export logic is duplicated here and the console application is not affected: without a
;; report-hook the engine behaves exactly as before.
(require racket/async-channel
         racket/string
         racket/list
         json
         "util.rkt"
         "cdp.rkt"
         "grok-export.rkt"
         (only-in "gemini.rkt" gemini-url?)
         (only-in "qwen.rkt" qwen-input?))

(provide (struct-out job)
         normalize-input
         input-problem
         build-argv
         start-job
         cancel-job!
         job-log!
         collect-result
         default-out-dir
         DEFAULT-OUT
         TOOL-NAME
         TOOL-VERSION)

;; ------------------------------------------------------------------ input validation
(define UUID-RX #px"^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$")

(define (default-out-dir) DEFAULT-OUT)

;; #f when acceptable, otherwise a plain-English problem description.
(define (input-problem s0)
  (define s (string-trim (or s0 "")))
  (cond
    [(string=? s "") "Enter a grok.com, gemini.google.com or chat.qwen.ai conversation URL (or \"qwen\" for the chat open in the Qwen app) first."]
    [(regexp-match? UUID-RX s) #f]
    [(gemini-url? s) #f]
    [(qwen-input? s) #f]
    [(regexp-match? #px"^https?://gemini\\.google\\.com/" s)
     "That Gemini URL is not a conversation. Expected https://gemini.google.com/app/<id>."]
    [(not (regexp-match? #px"^https?://" s))
     "That is not a URL or a conversation id. Expected https://grok.com/c/<id>, https://grok.com/share/<id>, or a bare uuid."]
    [(regexp-match? #px"^https?://[^/]*grok\\.com/(c|share)/[^/?#]+" s) #f]
    [(regexp-match? #px"^https?://[^/]*grok\\.com/" s)
     "That grok.com URL is neither a conversation (/c/<id>) nor a share link (/share/<id>)."]
    [else "That URL is not on grok.com. Expected https://grok.com/c/<id> or https://grok.com/share/<id>."]))

(define (normalize-input s) (normalize-url (string-trim s)))

;; ------------------------------------------------------------------ argv
;; Exactly the command line the console app would have been given.
(define (build-argv #:url url #:out out #:port port #:rounds rounds #:launch? launch? #:keep-open? keep-open?
                    #:skip-dom? [skip-dom? #f] #:no-attachments? [no-attachments? #f])
  (list->vector
   (append (list (string-trim url)
                 "--out" out
                 "--port" (number->string port)
                 "--rounds" (number->string rounds))
           (if launch? '() '("--no-launch"))
           (if keep-open? '("--keep-open") '())
           (if skip-dom? '("--skip-dom") '())
           (if no-attachments? '("--no-attachments") '()))))

;; ------------------------------------------------------------------ line-splitting output port
;; Every log-info/log-warn/printf in the engine ends up here; complete lines are pushed to the
;; async channel as they are produced.
(define (make-line-port emit)
  (define buf (open-output-bytes))
  (define lock (make-semaphore 1))
  (define (take-line!)
    (define bs (get-output-bytes buf #t))
    (emit (regexp-replace #rx"\r+$" (bytes->string/utf-8 bs #\uFFFD) "")))
  (make-output-port
   'grok-export-log
   always-evt
   (lambda (bstr start end non-block? breakable?)
     (call-with-semaphore
      lock
      (lambda ()
        (let loop ([i start])
          (when (< i end)
            (define nl (let scan ([j i])
                         (cond [(>= j end) #f]
                               [(= 10 (bytes-ref bstr j)) j]
                               [else (scan (add1 j))])))
            (cond
              [nl (write-bytes bstr buf i nl) (take-line!) (loop (add1 nl))]
              [else (write-bytes bstr buf i end)])))))
     (- end start))
   (lambda ()
     (call-with-semaphore
      lock
      (lambda ()
        (define bs (get-output-bytes buf #t))
        (unless (zero? (bytes-length bs))
          (emit (bytes->string/utf-8 bs #\uFFFD))))))))

;; ------------------------------------------------------------------ the job
;; cust    custodian owning the worker thread and every port/socket it opens
;; ch      async channel carrying (list kind payload): 'log 'phase 'export-dir 'tab 'login 'done 'result
;; tab     (list host port target-id) of the tab the run created, as soon as it exists
;; dir     the export directory, as soon as it exists
(struct job (cust thread ch [tab #:mutable] [dir #:mutable] [cancelled? #:mutable]) #:transparent)

(define (job-log! j fmt . args)
  (async-channel-put (job-ch j) (list 'log (apply format fmt args))))

(define (start-job argv)
  (define ch (make-async-channel))
  (define cust (make-custodian))
  (define th
    (parameterize ([current-custodian cust])
      (thread
       (lambda ()
         (define out (make-line-port (lambda (l) (async-channel-put ch (list 'log l)))))
         (define code
           (with-handlers ([exn:fail?
                            (lambda (e)
                              (async-channel-put ch (list 'log (string-append "ERROR: " (exn-message e))))
                              1)])
             (parameterize ([current-output-port out]
                            [current-error-port out]
                            ;; the manifest records the effective command line
                            [current-command-line-arguments argv]
                            [report-hook (lambda (kind payload)
                                           (async-channel-put ch (list kind payload)))])
               (main argv))))
         (with-handlers ([exn:fail? void]) (close-output-port out))
         (async-channel-put ch (list 'done code))))))
  (job cust th ch #f #f #f))

;; Hard stop: kill the worker (which closes its WebSocket and every file port it holds), then
;; close the tab the run created.  Whatever was already written to disk stays there.
(define (cancel-job! j)
  (set-job-cancelled?! j #t)
  (with-handlers ([exn:fail? void]) (custodian-shutdown-all (job-cust j)))
  (job-log! j "CANCELLED: the export worker and its Chrome connection were shut down.")
  (define tab (job-tab j))
  (cond
    [tab
     (thread
      (lambda ()
        (with-handlers ([exn:fail? (lambda (e) (job-log! j "Closing the tab failed: ~a" (exn-message e)))])
          (define-values (code body) (cdp-close-target (car tab) (cadr tab) (caddr tab)))
          (job-log! j "Closed the tab this run created (~a, HTTP ~a ~a)" (caddr tab) code (string-trim body)))
        (async-channel-put (job-ch j) (list 'cancel-finished #f))))]
    [else (async-channel-put (job-ch j) (list 'cancel-finished #f))])
  (void))

;; ------------------------------------------------------------------ result gathering
(define (read-json-file p)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (and (file-exists? p)
         (call-with-input-file p #:mode 'binary read-json))))

(define (h-ref h k [d #f])
  (if (hash? h) (hash-ref h k d) d))

(define (count-rollouts transcript)
  (define turns (h-ref transcript 'turns '()))
  (if (list? turns)
      (for/sum ([t (in-list turns)])
        (define r (h-ref (h-ref t 'thinking) 'rollouts '()))
        (if (list? r) (length r) 0))
      0))

;; Headline numbers for the completion banner, read back from the files the run wrote.
(define (collect-result dir exit-code)
  (define (p . xs) (and dir (apply build-path dir xs)))
  (define manifest (and dir (read-json-file (p "manifest.json"))))
  (define transcript (and dir (read-json-file (p "transcript.json"))))
  (define counts (h-ref manifest 'counts))
  (define dom (h-ref manifest 'domLane))
  (define rounds (h-ref dom 'rounds '()))
  (define stats (and (list? rounds) (pair? rounds) (h-ref (car rounds) 'stats)))
  (define ver (h-ref manifest 'verification))
  (define behavior (h-ref manifest 'behavior))
  (define behavior-summary (h-ref behavior 'summary))
  (define atts (h-ref manifest 'attachments '()))
  (define shots (h-ref dom 'screenshots '()))
  (define warns (h-ref manifest 'warnings '()))
  (define html (and dir (p "transcript.html")))
  (define behavior-json (and dir (p "behavior-report.json")))
  (define behavior-markdown (and dir (p "behavior-report.md")))
  (hasheq 'dir (and dir (if (path? dir) (path->string dir) dir))
          'exitCode exit-code
          'turns (or (h-ref counts 'turns) (and (hash? transcript) (length (h-ref transcript 'turns '()))) 0)
          'rollouts (count-rollouts transcript)
          'sourceRows (or (h-ref stats 'sourceRows) 0)
          'thoughtRows (or (h-ref stats 'thoughtRows) 0)
          'attachmentsExpected (or (h-ref counts 'attachments) 0)
          'attachmentsDownloaded (if (list? atts) (length atts) 0)
          'screenshots (if (list? shots) (length shots) 0)
          'checksTotal (or (h-ref ver 'total) 0)
          'checksOk (or (h-ref ver 'passed) 0)
          'checksFailed (or (h-ref ver 'failed) 0)
          'failedChecks (let ([f (h-ref ver 'failedChecks '())]) (if (list? f) f '()))
          'warnings (if (list? warns) (length warns) 0)
          'behaviorStatus (or (h-ref behavior 'status) "absent")
          'behaviorFindingCount (or (h-ref behavior-summary 'findingCount) 0)
          'behaviorConfirmed (or (h-ref behavior-summary 'confirmed) 0)
          'behaviorCandidate (or (h-ref behavior-summary 'candidate) 0)
          'behaviorNotAssessable (or (h-ref behavior-summary 'notAssessable) 0)
          'behaviorJson (and behavior-json (file-exists? behavior-json)
                             (path->string behavior-json))
          'behaviorMarkdown (and behavior-markdown (file-exists? behavior-markdown)
                                 (path->string behavior-markdown))
          'error (let ([e (h-ref manifest 'error)]) (if (string? e) e #f))
          'hasManifest (and manifest #t)
          'transcriptHtml (and html (file-exists? html) (path->string html))))
