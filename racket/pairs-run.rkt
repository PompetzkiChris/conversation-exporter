#lang racket/base
;; pairs-run.rkt — run the structural detectors over an exported transcript.json.
(require racket/list racket/string json "analysis.rkt" "pairs.rkt")

(define path (vector-ref (current-command-line-arguments) 0))
(define t (call-with-input-file path read-json))
(define turns (hash-ref t 'turns '()))

(define fs
  (let loop ([ts turns] [prev ""] [acc '()])
    (cond
      [(null? ts) (reverse acc)]
      [else
       (define turn (car ts))
       (define txt (hash-ref turn 'text ""))
       (if (equal? (hash-ref turn 'sender #f) "human")
           (loop (cdr ts) txt acc)
           (loop (cdr ts) prev
                 (append (reverse (analyse-pair (hash-ref turn 'index 0) prev txt)) acc)))])))

(printf "~a structural findings across ~a rules\n\n" (length fs) (pair-rule-count))
(define by-name (make-hash))
(for ([f (in-list fs)]) (hash-update! by-name (finding-name f) add1 0))
(for ([kv (in-list (sort (hash->list by-name) > #:key cdr))])
  (printf "~a  ~a\n" (cdr kv) (car kv)))
(printf "\n")
(for ([f (in-list fs)])
  (printf "turn ~a  [~a/~a]  ~a\n   \"~a\"\n"
          (finding-turn f) (category-label (finding-cat f)) (severity-label (finding-sev f))
          (finding-name f) (finding-quote f)))
