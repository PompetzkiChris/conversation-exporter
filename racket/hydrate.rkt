#lang racket/base
;; hydrate.rkt — detection of a server-side "unhydrated" API payload and a bounded re-fetch.
;;
;; Observed on 2026-09-02 17:15:12 (share lane, both endpoints, HTTP 200, valid JSON):
;; grok.com served the conversation with `xpostIds` populated (90 ids on turn 1) but
;; `xposts` empty and (legacy format) every xSearchResults list empty; an independent
;; fetch a few seconds later returned the full payload (41 posts).  The X-post hydration
;; step on grok's side had failed silently.  An export built from such a payload would be
;; missing every X post although the page shows them, so the app re-fetches (bounded) and
;; keeps every rejected attempt on disk as raw material.
(require "transcript.rkt")

(provide xposts-stripped?
         capture-unhydrated?
         transcript-xpost-ids
         fetch-until-hydrated)

;; -> #f when the payload looks complete, else a string describing the first inconsistency:
;; a response that lists xpostIds but carries no xposts.
(define (xposts-stripped? j)
  (and (hash? j)
       (for/or ([r (in-list (or-empty-list (jget j 'responses)))])
         (define ids (or-empty-list (jget r 'xpostIds)))
         (define posts (or-empty-list (jget r 'xposts)))
         (and (pair? ids)
              (null? posts)
              (format "response ~a lists ~a xpostIds but carries 0 xposts (X posts not hydrated by the server)"
                      (jget r 'responseId) (length ids))))))

;; fetch-once: thunk -> (values jsexpr-or-#f status bytes-or-#f).
;; Calls it up to max-attempts times while the payload is unhydrated, waiting delay-s
;; between attempts; on-reject is called with (attempt reason j status bytes) for every
;; rejected attempt so the caller can keep its bytes.
;; -> (values j status bytes attempts final-reason-or-#f)
(define (fetch-until-hydrated fetch-once
                              #:max-attempts [max-attempts 3]
                              #:delay-s [delay-s 2]
                              #:on-reject [on-reject void])
  (let loop ([attempt 1])
    (define-values (j status bs) (fetch-once))
    (define reason (and j (xposts-stripped? j)))
    (cond
      [(and reason (< attempt max-attempts))
       (on-reject attempt reason j status bs)
       (sleep delay-s)
       (loop (add1 attempt))]
      [else (values j status bs attempt reason)])))

;; The page renders from the same API: while the server serves the stripped payload, the
;; revealed X-post results in the Thoughts/Sources panels carry an empty user handle
;; (`https://x.com//status/<id>`) and a bare "@" title.  -> #f or a reason string.
;; known: the ids of the X posts the transcript carries with an author (transcript-xpost-ids).  When it is
;; non-empty, a post outside it is one the API also serves without an author (a deleted or unavailable post):
;; the page renders it with an empty handle however often it is reloaded, so it is not counted.
(define (capture-unhydrated? cap [known #f])
  (define n 0)
  (define (counts? u)
    (or (not known) (zero? (hash-count known))
        (let ([m (regexp-match #px"/status/([0-9]+)" u)]) (and m (hash-ref known (cadr m) #f)))))
  (define (scan-rows pan)
    (for* ([sec (in-list (or-empty-list (jget (or-empty-hash pan) 'sections)))]
           [r (in-list (or-empty-list (jget sec 'rows)))]
           [res (in-list (or-empty-list (jget r 'results)))])
      (define u (jget res 'url))
      (when (and (string? u) (regexp-match? #px"^https?://(?:www\\.)?x\\.com//status/" u) (counts? u))
        (set! n (add1 n)))))
  (when (hash? cap)
    (for ([key (in-list '(thoughtsByArticle sourcesByArticle))])
      (for ([(k pan) (in-hash (or-empty-hash (jget cap key)))])
        (scan-rows pan))))
  (and (> n 0)
       (format "~a revealed X-post result(s) have an empty user handle (https://x.com//status/...): the page was rendered from the unhydrated payload" n)))

;; transcript -> hash of postId -> #t for every X post object carrying a non-empty username
(define (transcript-xpost-ids t)
  (define ids (make-hash))
  (let walk ([v t])
    (cond
      [(hash? v)
       (define pid (hash-ref v 'postId #f))
       (define user (hash-ref v 'username #f))
       (when (and (string? pid) (string? user) (not (string=? user "")))
         (hash-set! ids pid #t))
       (for ([x (in-hash-values v)]) (walk x))]
      [(list? v) (for ([x (in-list v)]) (walk x))]
      [else (void)]))
  ids)