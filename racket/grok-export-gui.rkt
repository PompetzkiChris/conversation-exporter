#lang racket/gui
;; Native GUI front end for the existing Grok Heavy Racket exporter.
;;
;; The exporter stays in grok-export.rkt.  This module calls its exported
;; `main` function on a worker thread, captures the existing live log stream,
;; and presents it in a GRacket window.  Building with `raco exe --gui` keeps
;; the application free of a console window.

(require racket/class
         racket/file
         racket/port
         racket/string
         racket/system
         "grok-export.rkt")

(define APP-TITLE "Grok Export (Racket)")
;; the author's machine keeps exports in the project tree; everyone else gets Documents\Exporter\exports
(define DEFAULT-OUT
  (if (directory-exists? "C:\\ClaudeOutput\\grok-export-claude\\exports")
      "C:\\ClaudeOutput\\grok-export-claude\\exports"
      (path->string (build-path (find-system-path 'doc-dir) "Exporter" "exports"))))

(define running? #f)
(define last-output-dir DEFAULT-OUT)

(define frame
  (new frame%
       [label APP-TITLE]
       [width 860]
       [height 680]
       [min-width 650]
       [min-height 480]))

(define root
  (new vertical-panel%
       [parent frame]
       [alignment '(left top)]
       [border 16]
       [spacing 10]
       [stretchable-width #t]
       [stretchable-height #t]))

(new message%
     [parent root]
     [label "Export a complete Grok Heavy conversation, including expanded thoughts, sources, replies, and user inputs."]
     [stretchable-width #t])

(define url-field
  (new text-field%
       [parent root]
       [label "Grok URL or conversation ID"]
       [init-value ""]
       [stretchable-width #t]
       [callback
        (lambda (_field event)
          (when (eq? (send event get-event-type) 'text-field-enter)
            (start-export!)))]))

(define button-row
  (new horizontal-panel%
       [parent root]
       [alignment '(left center)]
       [spacing 8]
       [stretchable-width #t]
       [stretchable-height #f]))

(define export-button
  (new button%
       [parent button-row]
       [label "Export"]
       [callback (lambda (_button _event) (start-export!))]))

(define open-folder-button
  (new button%
       [parent button-row]
       [label "Open Export Folder"]
       [callback
        (lambda (_button _event)
          (define target
            (cond
              [(directory-exists? last-output-dir) last-output-dir]
              [(directory-exists? DEFAULT-OUT) DEFAULT-OUT]
              [else
               (make-directory* DEFAULT-OUT)
               DEFAULT-OUT]))
          (with-handlers
              ([exn:fail?
                (lambda (e)
                  (send status-message set-label
                        (format "Could not open the folder: ~a" (exn-message e))))])
            (shell-execute "explore" target "" (current-directory) 'sw_shownormal)))]))

(define status-message
  (new message%
       [parent root]
       [label "Ready."]
       [stretchable-width #t]))

(new message%
     [parent root]
     [label (format "Output root: ~a" DEFAULT-OUT)]
     [stretchable-width #t])

(define log-editor (new text%))
(send log-editor auto-wrap #t)
(send log-editor lock #t)

(define log-canvas
  (new editor-canvas%
       [parent root]
       [editor log-editor]
       [style '(auto-vscroll no-hscroll)]
       [stretchable-width #t]
       [stretchable-height #t]))

(define (set-running! value)
  (set! running? value)
  (send export-button enable (not value))
  (send url-field enable (not value)))

(define (clear-log!)
  (send log-editor lock #f)
  (send log-editor erase)
  (send log-editor lock #t))

(define (append-log! line)
  (send log-editor lock #f)
  (send log-editor insert (string-append line "\n") (send log-editor last-position))
  (define end (send log-editor last-position))
  (send log-editor set-position end end)
  (send log-editor scroll-to-position end)
  (send log-editor lock #t)
  (cond
    [(regexp-match #px"^EXPORT_DIR=(.+)$" line)
     => (lambda (m)
          (set! last-output-dir (string-trim (cadr m)))
          (send status-message set-label "Finalizing export files…"))]
    [(regexp-match? #px"^Navigating to " line)
     (send status-message set-label "Opening the Grok conversation…")]
    [(regexp-match? #px"DOM round|expanding|remainingCollapsed" line)
     (send status-message set-label "Expanding thoughts and sources…")]
    [(regexp-match? #px"verification\\.json|verification:" line)
     (send status-message set-label "Verifying the export…")]
    [(regexp-match? #px"attachments:" line)
     (send status-message set-label "Saving attachments…")]
    [else (void)]))

(define (finish-export! exit-code failure-message)
  (set-running! #f)
  (cond
    [failure-message
     (send status-message set-label (format "Export failed: ~a" failure-message))]
    [(zero? exit-code)
     (send status-message set-label "Complete — exported and verified.")]
    [(= exit-code 2)
     (send status-message set-label
           "Complete with verification warnings — inspect verification.json in the export folder.")]
    [else
     (send status-message set-label
           (format "Export failed with exit code ~a. See the progress log." exit-code))]))

(define (run-backend! input)
  (define-values (pipe-in pipe-out) (make-pipe 65536))
  (define reader
    (thread
     (lambda ()
       (let loop ()
         (define line (read-line pipe-in 'any))
         (unless (eof-object? line)
           (queue-callback (lambda () (append-log! line)) #f)
           (loop))))))
  (define exit-code 1)
  (define failure-message #f)
  (with-handlers
      ([exn:fail?
        (lambda (e)
          (set! failure-message (exn-message e))
          (fprintf pipe-out "ERROR: ~a\n" failure-message)
          (flush-output pipe-out))])
    (parameterize ([current-output-port pipe-out]
                   [current-error-port pipe-out])
      (set! exit-code (main (vector input)))))
  (close-output-port pipe-out)
  (sync reader)
  (close-input-port pipe-in)
  (queue-callback
   (lambda () (finish-export! exit-code failure-message))
   #f))

(define (start-export!)
  (unless running?
    (define input (string-trim (send url-field get-value)))
    (cond
      [(string=? input "")
       (send status-message set-label "Enter a Grok conversation URL or conversation ID.")
       (send url-field focus)]
      [else
       (clear-log!)
       (set! last-output-dir DEFAULT-OUT)
       (set-running! #t)
       (send status-message set-label "Starting export…")
       (thread (lambda () (run-backend! input)))])))

(send frame show #t)
(send url-field focus)

