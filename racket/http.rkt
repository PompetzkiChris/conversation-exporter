#lang racket/base
;; http.rkt — minimal HTTP/1.1 client over racket/tcp for the DevTools HTTP endpoint.
;; Reads the body by Content-Length (or chunked encoding); never waits for EOF when a
;; length is known, because the DevTools server keeps connections alive.
(require racket/tcp
         racket/string
         racket/port
         json)

(provide http-request
         http-get-json
         http-put-json
         read-http-headers)

;; Reads "Name: value" lines until the blank line; returns alist of (lowercase-name . value).
(define (read-http-headers in)
  (let loop ([acc '()])
    (define line (read-line in 'return-linefeed))
    (cond
      [(or (eof-object? line) (string=? line "")) (reverse acc)]
      [else
       (define m (regexp-match #px"^([^:]+):[ \t]*(.*)$" line))
       (loop (if m
                 (cons (cons (string-downcase (cadr m)) (string-trim (caddr m))) acc)
                 acc))])))

(define (read-exactly in n)
  (cond
    [(<= n 0) #""]
    [else
     (define b (read-bytes n in))
     (cond
       [(eof-object? b) #""]
       [else b])]))

(define (read-chunked in)
  (define out (open-output-bytes))
  (let loop ()
    (define line (read-line in 'return-linefeed))
    (unless (eof-object? line)
      (define size-str (car (string-split (string-trim line) ";")))
      (define size (string->number (string-trim size-str) 16))
      (cond
        [(or (not size) (zero? size))
         ;; trailers until blank line
         (let tl () (define l (read-line in 'return-linefeed))
           (unless (or (eof-object? l) (string=? l "")) (tl)))]
        [else
         (write-bytes (read-exactly in size) out)
         (read-line in 'return-linefeed)  ; CRLF after chunk
         (loop)])))
  (get-output-bytes out))

;; -> (values status-code headers body-bytes)
(define (http-request host port method path
                      #:body [body #f]
                      #:headers [extra-headers '()]
                      #:timeout [timeout 30])
  (define-values (in out) (tcp-connect host port))
  (dynamic-wind
   void
   (lambda ()
     (write-string (format "~a ~a HTTP/1.1\r\nHost: ~a:~a\r\nConnection: close\r\nAccept: */*\r\n"
                           method path host port) out)
     (for ([h (in-list extra-headers)])
       (write-string (format "~a: ~a\r\n" (car h) (cdr h)) out))
     (when body
       (write-string (format "Content-Length: ~a\r\n" (bytes-length body)) out))
     (write-string "\r\n" out)
     (when body (write-bytes body out))
     (flush-output out)
     (define status-line
       (or (sync/timeout timeout (read-line-evt in 'return-linefeed))
           (error 'http "~a ~a: no response from ~a:~a within ~a s" method path host port timeout)))
     (when (eof-object? status-line)
       (error 'http "~a ~a: connection closed by ~a:~a before a status line" method path host port))
     (define m (regexp-match #px"^HTTP/\\d\\.\\d[ \t]+(\\d{3})" status-line))
     (unless m (error 'http "~a ~a: malformed status line: ~s" method path status-line))
     (define code (string->number (cadr m)))
     (define headers (read-http-headers in))
     (define clen (let ([h (assoc "content-length" headers)]) (and h (string->number (string-trim (cdr h))))))
     (define chunked? (let ([h (assoc "transfer-encoding" headers)])
                       (and h (regexp-match? #rx"(?i:chunked)" (cdr h)))))
     (define body-bytes
       (cond
         [clen (read-exactly in clen)]
         [chunked? (read-chunked in)]
         [else (port->bytes in)]))   ; only when the server gave no length at all
     (values code headers body-bytes))
   (lambda ()
     (close-input-port in)
     (close-output-port out))))

(define (parse-json-body method path code body)
  (with-handlers ([exn:fail? (lambda (e)
                               (error 'http "~a ~a: status ~a, body is not JSON: ~a"
                                      method path code
                                      (let ([s (bytes->string/utf-8 body #\?)])
                                        (if (> (string-length s) 300) (substring s 0 300) s))))])
    (bytes->jsexpr body)))

;; GET and parse JSON; raises on non-2xx.
(define (http-get-json host port path #:timeout [timeout 30])
  (define-values (code headers body) (http-request host port "GET" path #:timeout timeout))
  (unless (and (>= code 200) (< code 300))
    (error 'http "GET ~a -> HTTP ~a: ~a" path code (bytes->string/utf-8 body #\?)))
  (parse-json-body "GET" path code body))

;; PUT (no body) and parse JSON; raises on non-2xx.
(define (http-put-json host port path #:timeout [timeout 30])
  (define-values (code headers body) (http-request host port "PUT" path #:body #"" #:timeout timeout))
  (unless (and (>= code 200) (< code 300))
    (error 'http "PUT ~a -> HTTP ~a: ~a" path code (bytes->string/utf-8 body #\?)))
  (parse-json-body "PUT" path code body))
