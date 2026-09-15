#lang racket/base
;; transcript.rkt — Grok API JSON (chunk or legacy format) -> normalized transcript jsexpr.
;; Byte-for-byte port of fixtures/reference_transcript.py `build`.  Python semantics
;; (dict.get -> None, `or` truthiness, key presence vs value) are reproduced with the
;; helpers exported below (jget/jhas?/truthy?/py-or/py-str).
(require racket/string
         racket/list
         json)

(provide build-transcript
         merge-ws-tool-results!
         transcript-format
         jget jhas? truthy? py-or py-str jstr
         or-empty-list or-empty-hash
         parse-time-us
         duration-ms)

(define ASSET-BASE "https://assets.grok.com/")

;; SPEC 0.6 / reference SOLO_ROLLOUT: single-agent modes (Build, Fast, Expert) report
;; uiLayout.rolloutIds == [] — there is no committee, so every event belongs to one
;; implicit rollout with this name and role null.
(define SOLO-ROLLOUT "Grok")

;; ------------------------------------------------------------------ python-ish helpers
;; dict.get(k): missing key or JSON null both become 'null.
(define (jget h k)
  (if (and (hash? h) (hash-has-key? h k)) (hash-ref h k) 'null))

;; `k in d`
(define (jhas? h k) (and (hash? h) (hash-has-key? h k)))

;; Python truthiness for JSON values.
(define (truthy? v)
  (not (or (eq? v 'null)
           (eq? v #f)
           (and (string? v) (string=? v ""))
           (and (number? v) (zero? v))
           (null? v)
           (and (hash? v) (zero? (hash-count v))))))

;; `a or b or c` — first truthy value, else the last one.
(define (py-or . vs)
  (let loop ([vs vs])
    (cond [(null? (cdr vs)) (car vs)]
          [(truthy? (car vs)) (car vs)]
          [else (loop (cdr vs))])))

;; str(v) as Python would render it inside "%s".
(define (py-str v)
  (cond [(string? v) v]
        [(eq? v 'null) "None"]
        [(eq? v #t) "True"]
        [(eq? v #f) "False"]
        [(exact-integer? v) (number->string v)]
        [(number? v) (number->string v)]
        [(symbol? v) (symbol->string v)]
        [(list? v) (string-append "[" (string-join (map py-repr v) ", ") "]")]
        [(hash? v) (string-append "{" (string-join
                                       (for/list ([k (sort (map (lambda (k) (if (symbol? k) (symbol->string k) k)) (hash-keys v)) string<?)])
                                         (format "'~a': ~a" k (py-repr (hash-ref v (if (hash-has-key? v k) k (string->symbol k))))))
                                       ", ") "}")]
        [else (format "~a" v)]))

(define (py-repr v)
  (if (string? v) (string-append "'" v "'") (py-str v)))

;; string-or-empty (Python `d.get("text", "")` with a string expected)
(define (jstr v) (if (string? v) v ""))

(define (or-empty-list v) (if (and (truthy? v) (list? v)) v '()))
(define (or-empty-hash v) (if (and (truthy? v) (hash? v)) v (hasheq)))

(define (mk . kvs)
  (define h (make-hasheq))
  (let loop ([kvs kvs])
    (unless (null? kvs)
      (hash-set! h (car kvs) (cadr kvs))
      (loop (cddr kvs))))
  h)

;; ------------------------------------------------------------------ time
;; RFC3339 -> exact integer microseconds since the Unix epoch (UTC), or #f.
;; Mirrors reference parse_time: "Z" -> +00:00, fractional part truncated to 6 digits,
;; a timestamp without a zone is treated as UTC.
(define TIME-RX
  #px"^(\\d{4})-(\\d{2})-(\\d{2})(?:[T ](\\d{2}):(\\d{2})(?::(\\d{2})(?:[.,](\\d+))?)?)?(?:(Z|z)|([+-])(\\d{2}):?(\\d{2})(?::(\\d{2}))?)?$")

(define (days-from-civil y m d)
  (define y2 (if (<= m 2) (- y 1) y))
  (define era (quotient (if (>= y2 0) y2 (- y2 399)) 400))
  (define yoe (- y2 (* era 400)))
  (define mp (if (> m 2) (- m 3) (+ m 9)))
  (define doy (+ (quotient (+ (* 153 mp) 2) 5) (- d 1)))
  (define doe (+ (* yoe 365) (quotient yoe 4) (- (quotient yoe 100)) doy))
  (+ (* era 146097) doe -719468))

(define (parse-time-us t)
  (cond
    [(not (string? t)) #f]
    [(string=? t "") #f]
    [else
     (define m (regexp-match TIME-RX t))
     (cond
       [(not m) #f]
       [else
        (define (num s) (if s (string->number s) 0))
        (define y (num (list-ref m 1)))
        (define mo (num (list-ref m 2)))
        (define d (num (list-ref m 3)))
        (define hh (num (list-ref m 4)))
        (define mi (num (list-ref m 5)))
        (define ss (num (list-ref m 6)))
        (define frac-s (list-ref m 7))
        (define frac6 (if frac-s
                          (let* ([f (if (> (string-length frac-s) 6) (substring frac-s 0 6) frac-s)]
                                 [padded (string-append f (make-string (- 6 (string-length f)) #\0))])
                            (string->number padded))
                          0))
        (define sign (list-ref m 9))
        (define tz-h (num (list-ref m 10)))
        (define tz-m (num (list-ref m 11)))
        (define tz-s (num (list-ref m 12)))
        (define offset (* (if (equal? sign "-") -1 1) (+ (* tz-h 3600) (* tz-m 60) tz-s)))
        (cond
          [(or (< mo 1) (> mo 12) (< d 1) (> d 31) (> hh 23) (> mi 59) (> ss 59)
               (> tz-h 23) (> tz-m 59))
           #f]
          [else
           (define days (days-from-civil y mo d))
           (define secs (- (+ (* days 86400) (* hh 3600) (* mi 60) ss) offset))
           (+ (* secs 1000000) frac6)])])]))

;; Integer milliseconds between two timestamps, rounded exactly as Python does:
;; total_seconds() (a correctly rounded double) * 1000, then round-half-even.
(define (duration-ms a b)
  (define ta (parse-time-us a))
  (define tb (parse-time-us b))
  (if (and ta tb)
      (let* ([diff-us (- tb ta)]
             [secs (exact->inexact (/ diff-us 1000000))]
             [ms (* secs 1000.0)])
        (inexact->exact (round ms)))
      'null))

;; ------------------------------------------------------------------ helpers
(define SKIP-KEYS '(toolUsageCardId toolCallId))
(define KIND-PRIORITY
  '("webSearch" "xPost" "xSearch" "xUserSearch" "browsePage" "viewImage"
    "chatroomSend" "initTerminalSession" "conversationSearch"))

;; first_key(d): the (only) key that is not an id key.  Python takes the first key in
;; JSON order; read-json hashes are unordered, so when several candidates exist we pick
;; deterministically: known tool kinds first (in KIND-PRIORITY order), else the smallest
;; key by code point.  Every card/result in the fixtures has exactly one candidate.
(define (first-key d)
  (cond
    [(not (hash? d)) (values 'null (hasheq))]
    [else
     (define names
       (sort (for/list ([k (in-hash-keys d)] #:unless (memq k SKIP-KEYS)) (symbol->string k))
             string<?))
     (define pri (filter (lambda (n) (member n names)) KIND-PRIORITY))
     (define chosen (cond [(null? names) #f]
                          [(pair? pri) (car pri)]
                          [else (car names)]))
     (cond
       [(not chosen) (values 'null (hasheq))]
       [else
        (define v (hash-ref d (string->symbol chosen)))
        (values chosen (if (eq? v 'null) (hasheq) v))])]))

(define (norm-web items)
  (for/list ([w (in-list (or-empty-list items))])
    (hasheq 'url (jget w 'url)
            'title (jget w 'title)
            'preview (if (jhas? w 'snippet) (jget w 'snippet) (jget w 'preview)))))

(define (norm-x items)
  (for/list ([p (in-list (or-empty-list items))])
    (hasheq 'postId (jget p 'postId)
            'username (if (jhas? p 'userhandle) (jget p 'userhandle) (jget p 'username))
            'name (jget p 'name)
            'text (jget p 'text)
            'createTime (jget p 'createTime))))

;; card_args(body) -- the arguments of a tool card (reference card_args, 2026-09-02 22:05).
;;
;; Heavy-era cards nest them: {"webSearch": {"args": {"query": "..."}}}.  Build-mode cards put
;; the payload at the top level of the card body instead: initTerminalSession carries
;; {"previewUrl": "https://...grok-sandbox.com"} and mcp carries {"toolName", "toolArgsJson"}.
;; Taking only "args" (the rule until 22:05) silently discarded both.  Rule now: the "args"
;; object when there is one, else the whole body when it is non-empty, else 'null (so the legacy
;; caller falls back to the <xai:tool_args> CDATA payload).  Heavy output is unchanged because
;; every Heavy card body is either {"args": {...}} or {} -- verified on both fixtures.
(define (card-args body)
  (cond
    [(not (hash? body)) 'null]
    [(hash? (jget body 'args)) (jget body 'args)]
    [(positive? (hash-count body)) body]
    [else 'null]))

(define CDATA-RX #px"<xai:tool_args><!\\[CDATA\\[(.*?)\\]\\]></xai:tool_args>")

;; legacy steps: only the inner "args" object of the CDATA JSON counts.
(define (parse-card-args-from-xml text)
  (define m (and (string? text) (regexp-match CDATA-RX text)))
  (cond
    [(not m) (hasheq)]
    [else
     (define d (with-handlers ([exn:fail? (lambda (e) #f)])
                 (string->jsexpr (cadr m))))
     (if (and (hash? d) (hash? (jget d 'args))) (jget d 'args) (hasheq))]))

;; Follow the parent chain from the root(s); siblings keep array order; orphans appended.
(define (order-responses responses)
  (define by-id (make-hash))
  (for ([r (in-list responses)]) (hash-set! by-id (jget r 'responseId) r))
  (define children (make-hash))   ; parent -> ids (reversed)
  (for ([r (in-list responses)])
    (hash-update! children (jget r 'parentResponseId)
                  (lambda (l) (cons (jget r 'responseId) l)) '()))
  (define roots
    (for/list ([r (in-list responses)]
               #:unless (hash-has-key? by-id (jget r 'parentResponseId)))
      (jget r 'responseId)))
  (define seen (make-hash))
  (define ordered '())
  (let loop ([stack roots])
    (unless (null? stack)
      (define rid (car stack))
      (cond
        [(hash-has-key? seen rid) (loop (cdr stack))]
        [else
         (hash-set! seen rid #t)
         (set! ordered (cons (hash-ref by-id rid) ordered))
         (loop (append (reverse (hash-ref children rid '())) (cdr stack)))])))
  (for ([r (in-list responses)])
    (define rid (jget r 'responseId))
    (unless (hash-has-key? seen rid)
      (hash-set! seen rid #t)
      (set! ordered (cons r ordered))))
  (reverse ordered))

;; Ids not on the current branch.  Where a response has several children (an edited message, a regenerated
;; reply), the last child in array order is the version the page shows; every earlier child and all of its
;; descendants are off the branch.  Children of an unknown parent (roots) are never split.
(define (off-branch-ids responses)
  (define by-id (make-hash))
  (for ([r (in-list responses)]) (hash-set! by-id (jget r 'responseId) #t))
  (define children (make-hash))   ; parent -> ids (reversed)
  (for ([r (in-list responses)])
    (hash-update! children (jget r 'parentResponseId) (lambda (l) (cons (jget r 'responseId) l)) '()))
  (define off (make-hash))
  (let loop ([stack (for*/list ([(p ch) (in-hash children)]
                                #:when (and (hash-has-key? by-id p) (>= (length ch) 2))
                                [k (in-list (cdr ch))]
                                #:unless (equal? k (car ch)))
                      k)])
    (unless (null? stack)
      (define rid (car stack))
      (cond
        [(hash-has-key? off rid) (loop (cdr stack))]
        [else (hash-set! off rid #t)
              (loop (append (hash-ref children rid '()) (cdr stack)))])))
  off)

;; ------------------------------------------------------------------ rollouts
(struct rl (id role [events #:mutable]))          ; events stored reversed
(struct rollouts (ids main by-id [order #:mutable] cards [unattributed #:mutable]))

(define (make-rollouts ids)
  (define main (if (pair? ids) (car ids) 'null))
  (define R (rollouts ids main (make-hash) '() (make-hash) '()))
  (for ([id (in-list ids)]) (rollouts-get R id))
  R)

(define (rollouts-get R name0)
  (define name (py-or name0 (rollouts-main R) ""))
  (unless (hash-has-key? (rollouts-by-id R) name)
    (hash-set! (rollouts-by-id R) name
               (rl name
                   (if (null? (rollouts-ids R))
                       'null
                       (if (equal? name (rollouts-main R)) "Leader" "Agent"))
                   '()))
    (set-rollouts-order! R (cons name (rollouts-order R))))
  (hash-ref (rollouts-by-id R) name))

;; reference Rollouts._events: an event whose chunk/step carries no rolloutId is
;; "unattributed relative to a leader" ONLY when there is a leader.  With rolloutIds == []
;; (Build mode) there is no committee, so it belongs to the implicit SOLO-ROLLOUT instead;
;; without this a Build transcript keeps its messages but loses its ENTIRE tool trace.
(define (add-event! R rollout ev)
  (cond
    [(or (eq? rollout 'null) (equal? rollout ""))
     (if (null? (rollouts-ids R))
         (let ([r (rollouts-get R SOLO-ROLLOUT)])
           (set-rl-events! r (cons ev (rl-events r))))
         (set-rollouts-unattributed! R (cons ev (rollouts-unattributed R))))]
    [else
     (let ([r (rollouts-get R rollout)])
       (set-rl-events! r (cons ev (rl-events r))))]))

(define (add-summary! R rollout text)
  (add-event! R rollout (mk 'type "summary" 'text text)))

(define (add-text! R rollout channel text)
  (add-event! R rollout (mk 'type "text" 'channel channel 'text text)))

(define (add-tool! R rollout card-id kind args)
  (define ev (mk 'type "tool" 'kind kind 'toolCallId card-id
                 'args (py-or args (hasheq)) 'results 'null))
  (add-event! R rollout ev)
  (hash-set! (rollouts-cards R) card-id ev)
  ev)

(define (add-result! R rollout call-id results)
  (define ev (hash-ref (rollouts-cards R) call-id #f))
  (if ev
      (hash-set! ev 'results results)
      (add-event! R rollout (mk 'type "tool_result" 'toolCallId call-id 'results results))))

(define (add-unknown! R rollout raw)
  (add-event! R rollout (mk 'type "unknown" 'raw raw)))

(define (rollouts-list R)
  (define main-key (py-or (rollouts-main R) ""))
  (define unattr (reverse (rollouts-unattributed R)))
  (for/list ([k (in-list (reverse (rollouts-order R)))])
    (define r (hash-ref (rollouts-by-id R) k))
    (define evs (reverse (rl-events r)))
    (hasheq 'id (rl-id r)
            'role (rl-role r)
            'events (if (and (equal? k main-key) (pair? unattr))
                        (append evs unattr)
                        evs))))

;; ------------------------------------------------------------------ per-format
(define (assistant-from-chunks r R)
  (define parts '())
  (define pos 0)
  (define cits '())
  (define main (rollouts-main R))
  (for ([c (in-list (or-empty-list (jget r 'outputChunks)))])
    (define meta (or-empty-hash (jget c 'metadata)))
    (define rname (py-or (jget meta 'rolloutId) 'null))
    (define main? (or (equal? rname main) (eq? rname 'null)))
    (cond
      [(jhas? c 'text)
       (define t (jget c 'text))
       (define ch (py-or (jget t 'channel) ""))
       (define txt (jstr (jget t 'text)))
       (cond
         [(equal? ch "CHANNEL_ASSISTANT_RESPONSE")
          (when main?
            (set! parts (cons txt parts))
            (set! pos (+ pos (string-length txt))))]
         [(equal? ch "CHANNEL_ASSISTANT_NOTETAKER_SUMMARY")
          (add-summary! R rname txt)]
         [else (add-text! R rname ch txt)])]
      [(jhas? c 'toolUsageCard)
       (define card (jget c 'toolUsageCard))
       (define-values (kind body) (first-key card))
       (define args (card-args body))
       (add-tool! R rname (jget card 'toolUsageCardId) kind args)]
      [(jhas? c 'toolResult)
       (define res (jget c 'toolResult))
       (define-values (kind body) (first-key res))
       (define payload
         (cond
           [(equal? kind "webSearch")
            (hasheq 'kind "web" 'items (norm-web (jget body 'webpages)))]
           [(equal? kind "xPost")
            (hasheq 'kind "x" 'items (norm-x (jget body 'posts)))]
           [(eq? kind 'null) 'null]
           [else (hasheq 'kind kind 'items '())]))
       (add-result! R rname (jget res 'toolCallId) payload)]
      [(jhas? c 'renderCitation)
       (define cit (jget c 'renderCitation))
       (when main?
         (define cid (jget cit 'citationId))
         (set! cits (cons (hasheq 'offset pos
                                  'citationId (if (eq? cid 'null) 0 cid)
                                  'cardId (jget cit 'id)
                                  'kind (jget cit 'kind)
                                  'url (jget cit 'url))
                          cits)))]
      [(jhas? c 'uiLayout) (void)]
      [else (add-unknown! R rname c)]))
  (values (apply string-append (reverse parts)) (reverse cits) '()))

(define CITE-RX
  #px"<grok:render card_id=\"([0-9a-fA-F]+)\" card_type=\"citation_card\" type=\"render_inline_citation\"><argument name=\"citation_id\">([0-9]+)</argument></grok:render>")

(define (join-text v)
  (apply string-append (map jstr (or-empty-list v))))

(define (assistant-from-steps r R)
  (for ([s (in-list (or-empty-list (jget r 'steps)))])
    (define rname (py-or (jget s 'rolloutId) 'null))
    (define tags (or-empty-list (jget s 'tags)))
    (cond
      [(member "summary" tags)
       (add-summary! R rname (join-text (jget s 'text)))]
      [(member "tool_usage_card" tags)
       (define xml (join-text (jget s 'text)))
       (for ([card (in-list (or-empty-list (jget s 'toolUsageCards)))])
         (define-values (kind body) (first-key card))
         (define args0 (card-args body))
         (define args (if (eq? args0 'null) (parse-card-args-from-xml xml) args0))
         (add-tool! R rname (jget card 'toolUsageCardId) kind args))
       (for ([t (in-list (or-empty-list (jget s 'toolUsageResults)))])
         (define payload
           (cond
             [(jhas? t 'webSearchResults)
              (hasheq 'kind "web"
                      'items (norm-web (jget (or-empty-hash (jget t 'webSearchResults)) 'results)))]
             [(jhas? t 'xSearchResults)
              (hasheq 'kind "x"
                      'items (norm-x (jget (or-empty-hash (jget t 'xSearchResults)) 'results)))]
             [else
              (define-values (k _b) (first-key t))
              (hasheq 'kind k 'items '())]))
         (add-result! R rname (jget t 'toolUsageCardId) payload))]
      [(member "raw_function_result" tags) (void)]
      [else
       (add-text! R rname (string-join (map py-str tags) ",") (join-text (jget s 'text)))]))
  ;; citation cards (SPEC 0.5): legacy responses carry cardAttachmentsJson = list of JSON strings
  ;; {"id":"97d2b7","type":"render_inline_citation","cardType":"citation_card","url":"https://…","kind":1}
  ;; keyed by the card_id of the inline markup (lower-cased hex); reference: assistant_from_steps.
  (define cards (citation-card-map r))
  ;; reply text: strip citation markup, remember offsets; resolve url/kind through the card map
  (define msg (let ([m (py-or (jget r 'message) "")]) (if (string? m) m (py-str m))))
  (define matches (regexp-match-positions* CITE-RX msg #:match-select values))
  (define out (open-output-string))
  (define cits '())
  (let loop ([ms matches] [last 0] [pos 0])
    (cond
      [(null? ms) (write-string msg out last (string-length msg))]
      [else
       (define m (car ms))
       (define whole (car m))
       (define g1 (cadr m))
       (define g2 (caddr m))
       (define seg-len (- (car whole) last))
       (write-string msg out last (car whole))
       (define pos* (+ pos seg-len))
       (define card-id (string-downcase (substring msg (car g1) (cdr g1))))
       (define card (hash-ref cards card-id #f))
       (set! cits (cons (hasheq 'offset pos*
                                'citationId (string->number (substring msg (car g2) (cdr g2)))
                                'cardId card-id
                                'kind (if card (citation-kind-name (jget card 'kind)) 'null)
                                'url (if card (jget card 'url) 'null))
                        cits))
       (loop (cdr ms) (cdr whole) pos*)]))
  (strip-images (get-output-string out) (reverse cits) cards))

;; searched-image cards in legacy reply text: <grok:render card_id="68042e" card_type="image_card"
;; type="render_searched_image"><argument name="image_id">W9RU3</argument>…</grok:render>
(define IMG-RX
  #px"<grok:render card_id=\"([0-9a-fA-F]+)\" card_type=\"image_card\" type=\"render_searched_image\">((?:<argument name=\"[a-z_]+\">[^<]*</argument>)*)</grok:render>")
(define ARG-RX #px"<argument name=\"([a-z_]+)\">([^<]*)</argument>")

;; Remove image-card markup from the (citation-stripped) reply text -> (values text citations images).
;; Each card becomes an images entry at its character offset; citation offsets at or after the end of a
;; card move back by its length.  Reference: strip_images.
(define (strip-images text cits cards)
  (define ms (regexp-match-positions* IMG-RX text #:match-select values))
  (define out (open-output-string))
  (define-values (images cuts)
    (for/fold ([imgs '()] [cuts '()] [last 0] [removed 0] #:result (values (reverse imgs) cuts))
              ([m (in-list ms)])
      (define whole (car m))
      (write-string text out last (car whole))
      (define cid (string-downcase (substring text (car (cadr m)) (cdr (cadr m)))))
      (define args (for/hash ([a (in-list (regexp-match* ARG-RX (substring text (car (caddr m)) (cdr (caddr m))) #:match-select cdr))])
                     (values (car a) (cadr a))))
      (define card (hash-ref cards cid #f))
      (define img (if (and card (hash? (jget card 'image))) (jget card 'image) (hasheq)))
      (define n (- (cdr whole) (car whole)))
      (values (cons (hasheq 'offset (- (car whole) removed) 'cardId cid
                            'imageId (hash-ref args "image_id" 'null) 'size (hash-ref args "size" 'null)
                            'title (jget img 'title) 'source (jget img 'source) 'link (jget img 'link)
                            'url (jget img 'original) 'thumbnail (jget img 'thumbnail))
                    imgs)
              (cons (cons (cdr whole) n) cuts)
              (cdr whole)
              (+ removed n))))
  (write-string text out (if (null? ms) 0 (cdr (car (last ms)))))
  (values (get-output-string out)
          (for/list ([c (in-list cits)])
            (define o (jget c 'offset))
            (hash-set c 'offset (- o (for/sum ([cut (in-list cuts)] #:when (<= (car cut) o)) (cdr cut)))))
          images))

;; numeric citation kind in cardAttachmentsJson -> the enum name used by the chunk format's
;; renderCitation (reference CITATION_KINDS); any other value -> 'null.
(define (citation-kind-name k)
  (cond
    [(eqv? k 0) "CITATION_KIND_UNSPECIFIED"]
    [(eqv? k 1) "CITATION_KIND_WEB_PAGE"]
    [(eqv? k 2) "CITATION_KIND_X_POST"]
    [else 'null]))

;; cardAttachmentsJson -> hash: lower-cased card id -> card jsexpr.  Entries that are not valid
;; JSON, not objects, or have no (truthy) id are skipped, as in the reference.  A later entry
;; with the same id replaces an earlier one (dict assignment order).
(define (citation-card-map r)
  (define cards (make-hash))
  (for ([raw (in-list (or-empty-list (jget r 'cardAttachmentsJson)))])
    (define c
      (cond
        [(string? raw) (with-handlers ([exn:fail? (lambda (e) #f)]) (string->jsexpr raw))]
        [else raw]))
    (when (and (hash? c) (truthy? (jget c 'id)))
      (hash-set! cards (string-downcase (py-str (jget c 'id))) c)))
  cards)

;; ------------------------------------------------------------------ build
(define (turn-attachments r)
  (define metas (make-hash))
  (for ([m (in-list (or-empty-list (jget r 'fileAttachmentsMetadata)))])
    (hash-set! metas (jget m 'fileMetadataId) m))
  (define assets (make-hash))
  (for ([a (in-list (or-empty-list (jget r 'fileAttachmentAssetMetadata)))])
    (hash-set! assets (jget a 'assetId) a))
  (for/list ([fid (in-list (or-empty-list (jget r 'fileAttachments)))])
    (define m (hash-ref metas fid (hasheq)))
    (define a (hash-ref assets fid (hasheq)))
    (define key (py-or (jget a 'key) (jget m 'fileUri) ""))
    (define pk (jget a 'previewImageKey))
    (hasheq 'fileId fid
            'fileName (py-or (jget m 'fileName) (jget a 'name) "")
            'mimeType (py-or (jget m 'fileMimeType) (jget a 'mimeType) "")
            'sizeBytes (jget a 'sizeBytes)
            'contentUrl (if (truthy? key) (string-append ASSET-BASE (py-str key)) 'null)
            'previewUrl (if (truthy? pk) (string-append ASSET-BASE (py-str pk)) 'null)
            'createTime (py-or (jget m 'createTime) (jget a 'createTime)))))

(define (build-turn r idx)
  (define sender (jget r 'sender))
  (define turn
    (mk 'index idx
        'responseId (jget r 'responseId)
        'parentResponseId (jget r 'parentResponseId)
        'sender sender
        'createTime (jget r 'createTime)
        'model (py-or (jget r 'model) 'null)
        'text ""
        'citations '()
        'attachments (turn-attachments r)
        'thinking 'null
        'sources 'null))
  ;; Image-generation result fields live beside fileAttachments in both response formats.  Add
  ;; them only when non-empty so every pre-existing transcript remains byte-identical, while a
  ;; response that actually has values retains the raw arrays without lossy reshaping.
  (for ([k (in-list '(generatedImageUrls imageEditUris imageAttachments))])
    (define v (jget r k))
    (when (truthy? v) (hash-set! turn k v)))
  (cond
    [(equal? sender "human")
     (define chunks (or-empty-list (jget r 'inputChunks)))
     (define text
       (if (null? chunks)
           (let ([m (py-or (jget r 'message) "")]) (if (string? m) m (py-str m)))
           (apply string-append
                  (for/list ([c (in-list chunks)] #:when (jhas? c 'text))
                    (jstr (jget (jget c 'text) 'text))))))
     (hash-set! turn 'text text)]
    [else
     (define layout0 (or-empty-hash (jget r 'uiLayout)))
     (define layout1
       (for/fold ([l layout0]) ([c (in-list (or-empty-list (jget r 'outputChunks)))])
         (if (jhas? c 'uiLayout) (jget c 'uiLayout) l)))
     (define layout
       (if (truthy? (jget layout1 'rolloutIds))
           layout1
           (hasheq 'rolloutIds
                   (or-empty-list
                    (jget (or-empty-hash (jget (or-empty-hash (jget r 'metadata)) 'ui_layout))
                          'rollout_ids)))))
     (define R (make-rollouts (or-empty-list (jget layout 'rolloutIds))))
     (define-values (text citations images)
       (if (truthy? (jget r 'outputChunks))
           (assistant-from-chunks r R)
           (assistant-from-steps r R)))
     (hash-set! turn 'text text)
     (hash-set! turn 'citations citations)
     (when (pair? images) (hash-set! turn 'images images))
     (define rlist (rollouts-list R))
     (hash-set! turn 'thinking
                (hasheq 'startTime (jget r 'thinkingStartTime)
                        'endTime (jget r 'thinkingEndTime)
                        'durationMs (duration-ms (jget r 'thinkingStartTime) (jget r 'thinkingEndTime))
                        'mainRollout (rollouts-main R)
                        'rollouts rlist))
     (define rows
       (for*/sum ([rl (in-list rlist)]
                  [ev (in-list (hash-ref rl 'events))])
         (define res (jget ev 'results))
         (if (truthy? res) (length (or-empty-list (jget res 'items))) 0)))
     (hash-set! turn 'sources
                (hasheq 'webSearchResults (norm-web (jget r 'webSearchResults))
                        'xposts (norm-x (jget r 'xposts))
                        'toolResultRows rows))])
  turn)

;; d: parsed API JSON (jsexpr); source-url: string or #f.
(define (build-transcript d source-url)
  (define conv (or-empty-hash (jget d 'conversation)))
  (define out-conv
    (hasheq 'conversationId (jget conv 'conversationId)
            'title (jget conv 'title)
            'createTime (jget conv 'createTime)
            'modifyTime (jget conv 'modifyTime)
            'sourceUrl (if (string? source-url) source-url 'null)
            'isPublic (if (jhas? d 'isPublic) (jget d 'isPublic) 'null)))
  (define responses (or-empty-list (jget d 'responses)))
  (define off (off-branch-ids responses))
  (define ordered (order-responses responses))
  (define (on? r) (not (hash-has-key? off (jget r 'responseId))))
  (define turns
    (for/list ([r (in-list (filter on? ordered))] [idx (in-naturals)])
      (build-turn r idx)))
  (define others
    (for/list ([r (in-list (filter (lambda (r) (not (on? r))) ordered))] [idx (in-naturals)])
      (build-turn r idx)))
  (if (null? others)
      (hasheq 'conversation out-conv 'turns turns)
      (hasheq 'conversation out-conv 'turns turns 'offBranchTurns others)))

;; ------------------------------------------------------------------ WebSocket tool results
;; grok.com's REST responses carry no output for agent tools such as bash (their results are null).  The page
;; receives it over wss://grok.com/ws/mgw as conversation.history.item events, whose item.x_grok.output_chunks[]
;; hold tool_result {tool_call_id, code_execution {stdout, exit_code}}.  frames: the parsed frames the page got.
;; Fills the results of tool events that have none with {kind "code", items [], stdout, exitCode}; -> count.
(define (merge-ws-tool-results! t frames)
  (define by-id (make-hash))
  (for ([f (in-list frames)])
    (define ev (or-empty-hash (jget (or-empty-hash f) 'event)))
    (when (equal? (jget ev 'type) "conversation.history.item")
      (define xg (or-empty-hash (jget (or-empty-hash (jget ev 'item)) 'x_grok)))
      (for ([ch (in-list (or-empty-list (jget xg 'output_chunks)))])
        (define tr (jget (or-empty-hash ch) 'tool_result))
        (when (and (hash? tr) (string? (jget tr 'tool_call_id)) (hash? (jget tr 'code_execution)))
          (hash-set! by-id (jget tr 'tool_call_id) (jget tr 'code_execution))))))
  (for*/sum ([turn (in-list (append (or-empty-list (jget t 'turns)) (or-empty-list (jget t 'offBranchTurns))))]
             [rl (in-list (or-empty-list (jget (or-empty-hash (jget turn 'thinking)) 'rollouts)))]
             [ev (in-list (or-empty-list (jget rl 'events)))])
    (define ce (and (hash? ev) (equal? (jget ev 'type) "tool") (eq? (jget ev 'results) 'null)
                    (hash-ref by-id (jget ev 'toolCallId) #f)))
    (cond
      [(and ce (not (immutable? ev)))
       (hash-set! ev 'results (hasheq 'kind "code" 'items '() 'stdout (jget ce 'stdout) 'exitCode (jget ce 'exit_code)))
       1]
      [else 0])))

;; "chunk" when at least one response has a non-empty outputChunks, else "legacy".
(define (transcript-format d)
  (if (for/or ([r (in-list (or-empty-list (jget d 'responses)))])
        (truthy? (jget r 'outputChunks)))
      "chunk"
      "legacy"))
