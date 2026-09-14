#lang racket/base
;; launch.rkt — detached Chrome launch with --remote-debugging-port and /json/version polling.
(require racket/system
         racket/file
         racket/list
         "http.rkt"
         "util.rkt")

(provide cdp-version
         default-chrome-path
         default-profile-dir
         launch-chrome!
         ensure-cdp!)

(define CHROME-PATH "C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe")
(define EDGE-PATH "C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe")

;; Chrome for all users, for this user only, 32-bit Program Files; Edge as the last resort
(define (default-chrome-path)
  (define (under var rel) (let ([d (getenv var)]) (and d (path->string (build-path d rel)))))
  (define candidates
    (filter values
            (list CHROME-PATH
                  (under "ProgramFiles" "Google\\Chrome\\Application\\chrome.exe")
                  (under "LOCALAPPDATA" "Google\\Chrome\\Application\\chrome.exe")
                  (under "ProgramFiles(x86)" "Google\\Chrome\\Application\\chrome.exe")
                  EDGE-PATH)))
  (or (for/first ([p (in-list candidates)] #:when (file-exists? p)) p) CHROME-PATH))

(define (default-profile-dir)
  (define lad (or (getenv "LOCALAPPDATA") "C:\\Temp"))
  (path->string (build-path lad "GrokExportClaude" "profile")))

;; GET /json/version -> jsexpr or #f
(define (cdp-version port)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (http-get-json "127.0.0.1" port "/json/version" #:timeout 5)))

;; Spawn Chrome detached (stdio -> NUL, not tied to our custodian). Returns the pid.
(define (launch-chrome! chrome port profile)
  (unless (file-exists? chrome)
    (error 'launch "browser executable not found: ~a" chrome))
  (make-directory* profile)
  (define nul-out (open-output-file "NUL" #:exists 'append))
  (define nul-err (open-output-file "NUL" #:exists 'append))
  (define nul-in (open-input-file "NUL"))
  (define-values (proc o i e)
    (parameterize ([current-subprocess-custodian-mode #f])
      (subprocess nul-out nul-in nul-err
                  chrome
                  (format "--remote-debugging-port=~a" port)
                  (format "--user-data-dir=~a" profile)
                  "--no-first-run"
                  "--disable-background-timer-throttling"   ; the two DOM rounds run in two tabs of one window:
                  "--disable-renderer-backgrounding"        ; the hidden one must keep prompt timers
                  "--disable-backgrounding-occluded-windows"
                  "--no-default-browser-check"
                  "--hide-crash-restore-bubble"
                  "--new-window"
                  "about:blank")))
  (close-output-port nul-out)
  (close-output-port nul-err)
  (close-input-port nul-in)
  (subprocess-pid proc))

;; Make sure a DevTools endpoint answers on the port; launches Chrome unless no-launch?.
;; Returns the /json/version jsexpr.
(define (ensure-cdp! port chrome profile no-launch? #:poll-seconds [poll-seconds 30])
  (define v (cdp-version port))
  (cond
    [v v]
    [no-launch?
     (error 'launch "no DevTools endpoint at http://127.0.0.1:~a/json/version and --no-launch was given" port)]
    [else
     (log-info "Launching ~a with --remote-debugging-port=~a --user-data-dir=~a" chrome port profile)
     (define pid (launch-chrome! chrome port profile))
     (log-info "Browser pid ~a; waiting for the DevTools endpoint (up to ~a s)" pid poll-seconds)
     (define deadline (+ (now-ms) (* 1000 poll-seconds)))
     (let loop ()
       (define v2 (cdp-version port))
       (cond
         [v2 v2]
         [(> (now-ms) deadline)
          (error 'launch "DevTools endpoint http://127.0.0.1:~a/json/version did not come up within ~a s (pid ~a)"
                 port poll-seconds pid)]
         [else (sleep 0.5) (loop)]))]))
