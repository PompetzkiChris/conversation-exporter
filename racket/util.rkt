#lang racket/base
;; util.rkt — UTF-8 binary file I/O, directories, hashing, base64, timestamps, logging.
(require racket/file
         racket/port
         racket/date
         racket/string
         file/sha1
         net/base64)

(provide verbose?
         log-info
         log-verbose
         log-warn
         now-ms
         ensure-dir!
         read-file-bytes
         write-file-bytes
         write-file-string
         sha256-hex
         base64->bytes
         bytes->base64-string
         iso-now-utc
         dir-timestamp-local
         safe-file-name
         path->string*
         report-hook
         report!)

;; ------------------------------------------------------------------ embedding hook
;; A host (the GUI) installs a procedure here to receive structured progress events while the
;; engine runs.  The default is `void`, so the console application behaves exactly as before.
;;   kind          payload
;;   'phase        (list label-string fraction-0..1)
;;   'export-dir   the export directory as a string
;;   'tab          (list host port target-id)   -- the tab this run created
;;   'login        (list seconds-left profile-dir)
(define report-hook (make-parameter void))

(define (report! kind [payload #f])
  (with-handlers ([exn:fail? void])
    ((report-hook) kind payload)))

(define verbose? (make-parameter #f))

(define (log-info fmt . args)
  (apply printf fmt args)
  (newline)
  (flush-output))

(define (log-warn fmt . args)
  (display "WARNING: ")
  (apply printf fmt args)
  (newline)
  (flush-output))

(define (log-verbose fmt . args)
  (when (verbose?)
    (display "  [v] ")
    (apply printf fmt args)
    (newline)
    (flush-output)))

;; Wall-clock milliseconds as an exact integer.
(define (now-ms) (inexact->exact (floor (current-inexact-milliseconds))))

(define (ensure-dir! p) (make-directory* p) p)

(define (path->string* p) (if (path? p) (path->string p) p))

;; Whole file as bytes (binary mode, no newline translation).
(define (read-file-bytes path)
  (call-with-input-file path #:mode 'binary port->bytes))

;; Write bytes verbatim; creates parent directories.
(define (write-file-bytes path bs)
  (define-values (base name dir?) (split-path path))
  (when (path? base) (make-directory* base))
  (call-with-output-file path
    #:mode 'binary
    #:exists 'truncate/replace
    (lambda (out) (write-bytes bs out) (void)))
  (bytes-length bs))

;; Write a string as UTF-8 (no BOM, no newline translation).
(define (write-file-string path s)
  (write-file-bytes path (string->bytes/utf-8 s)))

(define (sha256-hex bs) (bytes->hex-string (sha256-bytes bs)))

(define (base64->bytes s)
  (base64-decode (if (string? s) (string->bytes/utf-8 s) s)))

(define (bytes->base64-string bs)
  (bytes->string/utf-8 (base64-encode bs #"")))

(define (pad n width)
  (define s (number->string n))
  (string-append (make-string (max 0 (- width (string-length s))) #\0) s))

;; 2026-09-02T20:26:16.353Z
(define (iso-now-utc)
  (define ms (current-inexact-milliseconds))
  (define secs (inexact->exact (floor (/ ms 1000))))
  (define frac (inexact->exact (floor (- ms (* secs 1000.0)))))
  (define d (seconds->date secs #f))
  (format "~a-~a-~aT~a:~a:~a.~aZ"
          (date-year d) (pad (date-month d) 2) (pad (date-day d) 2)
          (pad (date-hour d) 2) (pad (date-minute d) 2) (pad (date-second d) 2)
          (pad (min 999 (max 0 frac)) 3)))

;; yyyymmdd-hhmmss in local time (export directory prefix).
(define (dir-timestamp-local)
  (define d (seconds->date (current-seconds) #t))
  (format "~a~a~a-~a~a~a"
          (date-year d) (pad (date-month d) 2) (pad (date-day d) 2)
          (pad (date-hour d) 2) (pad (date-minute d) 2) (pad (date-second d) 2)))

;; Strip characters that are illegal in Windows file names.
(define (safe-file-name s)
  (define cleaned
    (list->string
     (for/list ([c (in-string s)])
       (if (or (memv c '(#\\ #\/ #\: #\* #\? #\" #\< #\> #\|))
               (< (char->integer c) 32))
           #\_
           c))))
  (define trimmed (string-trim cleaned))
  (if (string=? trimmed "") "file" trimmed))
