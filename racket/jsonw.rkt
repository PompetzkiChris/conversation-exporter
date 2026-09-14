#lang racket/base
;; jsonw.rkt — canonical JSON writer (does NOT use write-json).
;;
;; Rules (from fixtures/reference_transcript.py):
;;  - objects: keys sorted by Unicode code point
;;  - indent = 1 space per nesting level, item separator ",\n", key separator ": "
;;  - empty object -> "{}", empty array -> "[]"
;;  - strings: escape " \ and control chars < 0x20 as \" \\ \n \r \t \b \f, else \u00XX
;;    (lowercase hex); everything else (non-ASCII, DEL, U+2028) written raw UTF-8
;;  - numbers: exact integers only; booleans true/false; null
;;  - the document ends with a trailing newline (jsexpr->canonical-string adds it)
(provide write-canonical-json
         jsexpr->canonical-string
         jsexpr->canonical-bytes
         write-json-string-literal)

(define (write-indent n out)
  (let loop ([i 0])
    (when (< i n)
      (write-char #\space out)
      (loop (add1 i)))))

(define hex-digits "0123456789abcdef")

(define (write-json-string-literal s out)
  (write-char #\" out)
  (define n (string-length s))
  ;; write runs of plain characters with one write-string call each
  (let loop ([i 0] [run-start 0])
    (cond
      [(= i n)
       (when (< run-start n) (write-string s out run-start n))]
      [else
       (define c (string-ref s i))
       (define code (char->integer c))
       (cond
         [(or (char=? c #\") (char=? c #\\) (< code 32))
          (when (< run-start i) (write-string s out run-start i))
          (cond
            [(char=? c #\") (write-string "\\\"" out)]
            [(char=? c #\\) (write-string "\\\\" out)]
            [(= code 10) (write-string "\\n" out)]
            [(= code 13) (write-string "\\r" out)]
            [(= code 9) (write-string "\\t" out)]
            [(= code 8) (write-string "\\b" out)]
            [(= code 12) (write-string "\\f" out)]
            [else
             (write-string "\\u00" out)
             (write-char (string-ref hex-digits (quotient code 16)) out)
             (write-char (string-ref hex-digits (remainder code 16)) out)])
          (loop (add1 i) (add1 i))]
         [else (loop (add1 i) run-start)])]))
  (write-char #\" out))

(define (key->string k)
  (cond [(symbol? k) (symbol->string k)]
        [(string? k) k]
        [else (error 'canonical-json "object key is not a symbol or string: ~s" k)]))

(define (write-canonical-json v out [level 0])
  (cond
    [(eq? v 'null) (write-string "null" out)]
    [(eq? v #t) (write-string "true" out)]
    [(eq? v #f) (write-string "false" out)]
    [(string? v) (write-json-string-literal v out)]
    [(exact-integer? v) (write-string (number->string v) out)]
    [(and (number? v) (integer? v)) (write-string (number->string (inexact->exact v)) out)]
    [(number? v) (error 'canonical-json "non-integer number is not allowed: ~a" v)]
    [(list? v)
     (cond
       [(null? v) (write-string "[]" out)]
       [else
        (write-char #\[ out)
        (write-char #\newline out)
        (let loop ([items v])
          (write-indent (add1 level) out)
          (write-canonical-json (car items) out (add1 level))
          (unless (null? (cdr items))
            (write-char #\, out)
            (write-char #\newline out)
            (loop (cdr items))))
        (write-char #\newline out)
        (write-indent level out)
        (write-char #\] out)])]
    [(hash? v)
     (cond
       [(zero? (hash-count v)) (write-string "{}" out)]
       [else
        (define keys (sort (for/list ([k (in-hash-keys v)]) (key->string k)) string<?))
        (write-char #\{ out)
        (write-char #\newline out)
        (let loop ([ks keys])
          (define k (car ks))
          (write-indent (add1 level) out)
          (write-json-string-literal k out)
          (write-string ": " out)
          (write-canonical-json (hash-ref v (if (hash-has-key? v k) k (string->symbol k))) out (add1 level))
          (unless (null? (cdr ks))
            (write-char #\, out)
            (write-char #\newline out)
            (loop (cdr ks))))
        (write-char #\newline out)
        (write-indent level out)
        (write-char #\} out)])]
    [(void? v) (write-string "null" out)]
    [else (error 'canonical-json "unsupported value: ~s" v)]))

;; Full document: canonical JSON + trailing LF.
(define (jsexpr->canonical-string v)
  (define out (open-output-string))
  (write-canonical-json v out 0)
  (write-char #\newline out)
  (get-output-string out))

(define (jsexpr->canonical-bytes v)
  (string->bytes/utf-8 (jsexpr->canonical-string v)))
