#lang racket/base
;; verify.rkt — SPEC.md section 5 verifier: a native port of shared/reference_verify.py.
;;
;; verify-capture      checks 1-10 and 12 (turn-count, user-text, assistant-text, attachments,
;;                     thought-label, rollouts, summaries, chatroom, tool-rows, citations,
;;                     expansion-complete) + the api-presence warnings; same record shapes,
;;                     same ok verdicts and the same warning strings as the Python reference.
;; stability-check     check 11 (dom-stability): canonical JSON of two captures minus
;;                     capturedAt / env / articles[*].html.
;; api-consistency-check  check 13: chunk vs legacy transcript differ only in citations[].url/kind.
;; enrich-citations    legacy lane: fill citations[].url/kind from the DOM chips (one-to-one only).
(require racket/string
         racket/list
         racket/vector
         "transcript.rkt"
         "jsonw.rkt"
         "util.rkt")

(provide verify-capture
         stability-check
         api-consistency-check
         enrich-citations
         strip-env
         norm
         md-words
         duration-seconds
         urlkey
         chip-size
         py-split)

;; ------------------------------------------------------------------ text normalization
;; Python str.isspace(): Unicode White_Space plus U+001C..U+001F.
(define (py-space? c)
  (or (char-whitespace? c)
      (let ([n (char->integer c)]) (and (>= n 28) (<= n 31)))))

;; Python str.split() (no argument): runs of whitespace separate, leading/trailing dropped.
(define (py-split s)
  (define n (string-length s))
  (let loop ([i 0] [start #f] [acc '()])
    (cond
      [(= i n) (reverse (if start (cons (substring s start i) acc) acc))]
      [(py-space? (string-ref s i))
       (loop (add1 i) #f (if start (cons (substring s start i) acc) acc))]
      [else (loop (add1 i) (or start i) acc)])))

;; Python str.strip(): trims Unicode whitespace at both ends, keeps the inside untouched.
(define (py-strip-ends s)
  (define n (string-length s))
  (let* ([a (let loop ([i 0]) (if (and (< i n) (py-space? (string-ref s i))) (loop (add1 i)) i))]
         [b (let loop ([i n]) (if (and (> i a) (py-space? (string-ref s (sub1 i)))) (loop (sub1 i)) i))])
    (substring s a b)))

(define (drop-cr-zw s)
  (list->string
   (for/list ([c (in-string s)]
              #:unless (memv c '(#\return #\u200B #\u2060 #\uFEFF)))
     c)))

(define (as-text v)
  (cond [(eq? v 'null) ""]
        [(string? v) v]
        [else (py-str v)]))

;; norm(s): NFC; \r removed; zero-width chars removed; whitespace runs -> one space; trimmed.
(define (norm v)
  (string-join (py-split (drop-cr-zw (string-normalize-nfc (as-text v)))) " "))

;; \( \) \[ \] $$ are math delimiters: the page shows the TeX between them (extract.js reads formulas as TeX)
(define MATH-DELIM   #px"\\\\[()\\[\\]]|\\$\\$")
(define TABLE-PIPE   #px"\\|")   ; markdown table pipes: the page renders a table, not the pipes
(define CODE-SPAN    #px"`([^`\n]*)`")   ; inline code: backslashes inside are literal, not markdown escapes
(define MD-LINK      #px"!?\\[([^\\]]*)\\]\\([^)]*\\)")
(define MD-RULE      #px"(?m:^[ \t]*(?:[-*_=][ \t]*){3,}$)")
(define MD-LINE-MARK #px"(?m:^[ \t]*(?:[-*+]|[0-9]+[.)])[ \t]+)")
(define MD-HEAD      #px"(?m:^[ \t]*#+[ \t]*)")
(define MD-QUOTE     #px"(?m:^[ \t]*>[ \t]*)")
(define MD-ESC       #px"\\\\([\\\\`*_{}\\[\\]()#+\\-.!>|~])")
(define MD-CHARS     #px"[*_#>`]")

;; md_words(s): markdown stripped (both sides identically), split into words.
(define (md-words v)
  (define s0 (as-text v))
  (define s1 (drop-cr-zw (string-normalize-nfc s0)))
  (define s2 (regexp-replace* TABLE-PIPE (regexp-replace* MD-LINK (regexp-replace* MATH-DELIM s1 "") "\\1") " "))
  (define s3 (regexp-replace* MD-RULE s2 ""))
  (define s4 (regexp-replace* MD-LINE-MARK s3 ""))
  (define s5 (regexp-replace* MD-HEAD s4 ""))
  (define s6 (regexp-replace* MD-QUOTE s5 ""))
  (define s6b (regexp-replace* CODE-SPAN s6 (lambda (all inner) (string-replace inner "\\" "\uE000"))))
  (define s7 (regexp-replace* MD-ESC s6b "\\1"))
  (define s8 (regexp-replace* MD-CHARS s7 ""))
  (py-split (string-replace s8 "\uE000" "\\")))

(define (sha-str s) (sha256-hex (string->bytes/utf-8 s)))

(define (str-head s n) (if (> (string-length s) n) (substring s 0 n) s))

(define (urlkey v)
  (define u0 (string-downcase (norm v)))
  (define u1 (regexp-replace #px"^[a-z]+://" u0 ""))
  (define u2 (regexp-replace #px"^www\\." u1 ""))
  (regexp-replace #px"/+$" u2 ""))

(define DUR-RX #px"(?:([0-9]+)h)?[ \t\n\r\f\v]*(?:([0-9]+)m)?[ \t\n\r\f\v]*(?:([0-9]+)s)?[ \t\n\r\f\v]*$")

;; 'Thought for 1m 21s' -> 81; supports h/m/s components; 'null when unparsable.
(define (duration-seconds label)
  (cond
    [(not (and (string? label) (truthy? label))) 'null]
    [else
     (define m (regexp-match DUR-RX (py-strip-ends label)))
     (cond
       [(or (not m) (not (or (cadr m) (caddr m) (cadddr m)))) 'null]
       [else
        (define (num s) (if s (string->number s) 0))
        (+ (* 3600 (num (cadr m))) (* 60 (num (caddr m))) (num (cadddr m)))])]))

(define (chip-size chip)
  (define m (regexp-match #px"\\+([0-9]+)[ \t\n\r\f\v]*$" (norm (jget chip 'text))))
  (+ 1 (if m (string->number (cadr m)) 0)))

;; Python slicing with clamping on a vector -> list
(define (vslice v a b)
  (define n (vector-length v))
  (define a* (max 0 (min n a)))
  (define b* (max a* (min n b)))
  (for/list ([i (in-range a* b*)]) (vector-ref v i)))

(define (py-list-repr l) (py-str l))

;; ------------------------------------------------------------------ report
(struct report ([checks #:mutable] [warnings #:mutable]))

(define (rep-add! rep name turn expected actual ok . extras)
  (define rec (make-hasheq))
  (hash-set! rec 'name name)
  (hash-set! rec 'turnIndex turn)
  (hash-set! rec 'expected expected)
  (hash-set! rec 'actual actual)
  (hash-set! rec 'ok (and ok #t))
  (let loop ([kv extras])
    (unless (null? kv)
      (hash-set! rec (car kv) (cadr kv))
      (loop (cddr kv))))
  (set-report-checks! rep (cons rec (report-checks rep)))
  rec)

(define (rep-warn! rep msg)
  (set-report-warnings! rep (cons msg (report-warnings rep))))

;; ------------------------------------------------------------------ capture / transcript access
(define (turns-of t) (or-empty-list (jget t 'turns)))
(define (arts-of cap) (or-empty-list (jget cap 'articles)))
(define (art-at arts i)
  (if (and (exact-integer? i) (< i (length arts))) (list-ref arts i) (hasheq)))

;; panel(cap, key, idx) -> jsexpr or 'null
(define (panel cap key idx)
  (define d (or-empty-hash (jget cap key)))
  (define k (string->symbol (number->string idx)))
  (if (hash-has-key? d k) (hash-ref d k) 'null))

;; list of (cons section row)
(define (dom-rows pan)
  (for*/list ([sec (in-list (or-empty-list (jget (or-empty-hash pan) 'sections)))]
              [r (in-list (or-empty-list (jget sec 'rows)))])
    (cons sec r)))

;; list of (cons rollout event)
(define (api-events turn)
  (define th (or-empty-hash (jget turn 'thinking)))
  (for*/list ([rl (in-list (or-empty-list (jget th 'rollouts)))]
              [ev (in-list (or-empty-list (jget rl 'events)))])
    (cons rl ev)))

(define (human? turn) (equal? (jget turn 'sender) "human"))
(define (assistant? turn) (equal? (jget turn 'sender) "assistant"))

(define PAGE-NOTE
  (string-append
   "observed grok.com behaviour (share page, 2026-09-02): chatroom messages whose outputChunk is streamed AFTER the last "
   "CHANNEL_ASSISTANT_RESPONSE chunk of the turn are not rendered in the Thoughts panel (test conversation turns 7 and 15); "
   "the API export keeps them, the page omits them"))

;; ------------------------------------------------------------------ checks
(define (check-turn-count! rep t cap)
  (define turns (turns-of t))
  (define arts (arts-of cap))
  (define exp (hasheq 'count (length turns)
                      'senders (for/list ([x (in-list turns)]) (if (human? x) "user" "assistant"))))
  (define act (hasheq 'count (length arts)
                      'senders (for/list ([a (in-list arts)]) (jget a 'role))))
  (rep-add! rep "turn-count" 'null exp act (equal? exp act)))

(define (first-diff ew dw [ctx 8])
  (define ev (list->vector ew))
  (define dv (list->vector dw))
  (define n (min (vector-length ev) (vector-length dv)))
  (define pos
    (or (for/first ([k (in-range n)] #:unless (equal? (vector-ref ev k) (vector-ref dv k))) k) n))
  (if (and (= pos (vector-length ev)) (= pos (vector-length dv)))
      'null
      (hasheq 'position pos
              'expected (vslice ev (max 0 (- pos ctx)) (+ pos ctx))
              'actual (vslice dv (max 0 (- pos ctx)) (+ pos ctx))
              'expectedWords (vector-length ev)
              'actualWords (vector-length dv))))

(define (check-user-text! rep t cap)
  (define arts (arts-of cap))
  (for ([turn (in-list (turns-of t))] #:when (human? turn))
    (define i (jget turn 'index))
    (define a (art-at arts i))
    (define e (norm (jget turn 'text)))
    (define d (norm (jget a 'text)))
    (define exact (string=? e d))
    (define ew (md-words (jget turn 'text)))
    (define dw (md-words (jget a 'text)))
    (define ok (or exact (equal? ew dw)))
    (rep-add! rep "user-text" i
              (hasheq 'chars (string-length e) 'sha256 (sha-str e) 'head (str-head e 80) 'words (length ew))
              (hasheq 'chars (string-length d) 'sha256 (sha-str d) 'head (str-head d 80) 'words (length dw))
              ok
              'mode (cond [exact "exact"] [ok "markdown-stripped"] [else "mismatch"])
              'firstDiff (if ok 'null (first-diff ew dw)))))

(define (subsequence? small big)
  (let loop ([s small] [b big])
    (cond [(null? s) #t]
          [(null? b) #f]
          [(equal? (car s) (car b)) (loop (cdr s) (cdr b))]
          [else (loop s (cdr b))])))

(define (missing-spans ew dw)
  (define ev (list->vector ew))
  (define dv (list->vector dw))
  (define ne (vector-length ev))
  (define nd (vector-length dv))
  (define (span-text a b) (str-head (string-join (vslice ev a b) " ") 300))
  (define spans '())
  (let loop ([i 0] [j 0] [start #f])
    (cond
      [(< i ne)
       (cond
         [(and (< j nd) (equal? (vector-ref ev i) (vector-ref dv j)))
          (when start
            (set! spans (cons (hasheq 'fromWord start 'toWord (- i 1) 'text (span-text start i)) spans)))
          (loop (add1 i) (add1 j) #f)]
         [else (loop (add1 i) j (or start i))])]
      [else
       (when start
         (set! spans (cons (hasheq 'fromWord start 'toWord (- ne 1) 'text (span-text start ne)) spans)))]))
  (define all (reverse spans))
  (if (> (length all) 20) (take all 20) all))

;; "team.An": the API glues consecutive reply messages without a space
;; "boy.”That": closing quotes or brackets may sit between the stop and the next capital
(define (split-glue ws) (py-split (regexp-replace* #px"([.!?][\"\u201D\u2019)\\]]*)(?=[A-Z])" (string-join ws " ") "\\1 ")))

(define (check-assistant-text! rep t cap)
  (define arts (arts-of cap))
  (for ([turn (in-list (turns-of t))] #:when (assistant? turn))
    (define i (jget turn 'index))
    (define a (art-at arts i))
    (define ew (md-words (jget turn 'text)))
    (define dw (md-words (jget a 'text)))
    (define equal-words (equal? ew dw))
    (define sub (or equal-words (subsequence? dw ew) (subsequence? (split-glue dw) (split-glue ew))))
    (define rec
      (rep-add! rep "assistant-text" i
                (hasheq 'words (length ew) 'sha256 (sha-str (string-join ew " ")))
                (hasheq 'words (length dw) 'sha256 (sha-str (string-join dw " ")))
                sub
                'firstDiff (if equal-words 'null (first-diff ew dw))
                'rule "DOM words == API words, or DOM words a subsequence of API words (page omission -> warning)"))
    (unless equal-words
      (hash-set! rec 'domIsSubsequenceOfApi sub)
      (hash-set! rec 'classification (if sub "page-omits-content" "mismatch"))
      (hash-set! rec 'missingFromDom (missing-spans ew dw))
      (when sub
        (rep-warn! rep (format "turn ~a: the page renders ~a of ~a reply words; the export keeps the full API text (see assistant-text.missingFromDom)"
                               i (length dw) (length ew)))))))

(define FILEID-RX #px"/([0-9a-f-]{36})/preview-image")

(define (attachment-file-name i a)
  (format "~a-~a-~a" i (safe-file-name (py-str (jget a 'fileId))) (safe-file-name (py-str (jget a 'fileName)))))

(define (check-attachments! rep t cap att-dir)
  (define arts (arts-of cap))
  (for ([turn (in-list (turns-of t))] #:when (human? turn))
    (define i (jget turn 'index))
    (define api (or-empty-list (jget turn 'attachments)))
    (define dom (or-empty-list (jget (art-at arts i) 'attachments)))
    (unless (and (null? api) (null? dom))
      (define exp-names (for/list ([x api]) (jget x 'fileName)))
      (define exp-ids (for/list ([x api]) (jget x 'fileId)))
      (define exp-prev (for/list ([x api]) (jget x 'previewUrl)))
      (define exp (hasheq 'names exp-names 'fileIds exp-ids 'previewUrls exp-prev
                          'sizes (for/list ([x api]) (jget x 'sizeBytes))))
      (define dom-ids
        (for/list ([d dom])
          (define m (regexp-match FILEID-RX (as-text (jget d 'previewSrc))))
          (if m (cadr m) 'null)))
      (define act-names (for/list ([d dom]) (jget d 'name)))
      (define act-prev (for/list ([d dom]) (jget d 'previewSrc)))
      (define size-ok #t)
      (define sizes
        (for/list ([x api])
          (cond
            [att-dir
             (define p (build-path att-dir (attachment-file-name i x)))
             (cond
               [(file-exists? p)
                (define sz (file-size p))
                (unless (equal? sz (jget x 'sizeBytes)) (set! size-ok #f))
                sz]
               [else (set! size-ok #f) 'null])]
            [else "not checked"])))
      (define act (hasheq 'names act-names 'fileIds dom-ids 'previewUrls act-prev 'sizes sizes))
      (define names-ok
        (and (= (length exp-names) (length act-names))
             (for/and ([e exp-names] [d act-names]) (or (eq? d 'null) (equal? d e)))))
      ;; a file without a preview (a PDF) has no element on the page that carries its id
      (define visible-ids (for/list ([x api]) (if (truthy? (jget x 'previewUrl)) (jget x 'fileId) 'null)))
      (define ok (and names-ok (equal? visible-ids dom-ids) (equal? exp-prev act-prev) size-ok))
      ;; names live only in a hover tooltip; a null DOM name means the tooltip did not render (hidden tab)
      ;; and is not an error and not a warning (SPEC 0.5): the identity is the fileId / preview URL.
      ;; The record says where the names came from (nameSource), as the reference verifier does.
      (rep-add! rep "attachments" i exp act ok
                'sizeCheck (if att-dir "files" "not performed (no --attachments DIR)")
                'nameSource (if (for/and ([d act-names]) (not (eq? d 'null)))
                                "dom tooltip"
                                "api (the page shows the name only in a hover tooltip)")
                'rule "fileIds equal where the API has a preview (the page shows the id only in the preview image URL); previewUrls equal; DOM names equal when present; sizes equal when files are given"))))

(define (check-thought-label! rep t cap)
  (define arts (arts-of cap))
  (for ([turn (in-list (turns-of t))] #:when (assistant? turn))
    (define i (jget turn 'index))
    (define th (or-empty-hash (jget turn 'thinking)))
    (define label (jget (art-at arts i) 'thoughtLabel))
    (define secs (duration-seconds label))
    (define dur (jget th 'durationMs))
    (define delta (if (or (eq? secs 'null) (eq? dur 'null) (not (exact-integer? dur)))
                      'null
                      (abs (- (* secs 1000) dur))))
    ;; the page shows no Thoughts label for a reply whose thinking produced no events
    (define no-events? (and (not (truthy? label))
                            (not (for/or ([rl (in-list (or-empty-list (jget th 'rollouts)))]) (truthy? (jget rl 'events))))))
    (define ok (or no-events? (and (not (eq? delta 'null)) (<= delta 2000))))
    (if no-events?
        (rep-add! rep "thought-label" i
                  (hasheq 'durationMs dur 'toleranceMs 2000)
                  (hasheq 'label label 'seconds secs 'deltaMs delta)
                  ok
                  'note "no thinking events in the API and no Thoughts label on the page")
        (rep-add! rep "thought-label" i
                  (hasheq 'durationMs dur 'toleranceMs 2000)
                  (hasheq 'label label 'seconds secs 'deltaMs delta)
                  ok))))

(define (sorted-unique-strings l)
  (sort (remove-duplicates l) string<?))

(define (check-rollouts! rep t cap)
  (for ([turn (in-list (turns-of t))] #:when (assistant? turn))
    (define i (jget turn 'index))
    (define th (or-empty-hash (jget turn 'thinking)))
    (define api (sorted-unique-strings
                 (for/list ([rl (in-list (or-empty-list (jget th 'rollouts)))]
                            #:when (truthy? (jget rl 'events)))
                   (py-str (jget rl 'id)))))
    (define pan (or-empty-hash (panel cap 'thoughtsByArticle i)))
    ;; a page that names no agent (canvas replies) shows the single rollout of a solo turn
    (define sole (and (= (length api) 1) (car api)))
    (define dom (sorted-unique-strings
                 (for/list ([sec (in-list (or-empty-list (jget pan 'sections)))]
                            #:when (or (truthy? (jget sec 'rollout)) sole))
                   (if (truthy? (jget sec 'rollout)) (py-str (jget sec 'rollout)) sole))))
    (define extra (filter (lambda (x) (not (member x api))) dom))
    (define missing (filter (lambda (x) (not (member x dom))) api))
    (define ok (null? extra))
    (define rec (rep-add! rep "rollouts" i api dom ok
                          'rule "every DOM rollout exists in the API; API rollouts absent from the page -> warning"))
    (unless (equal? api dom)
      (hash-set! rec 'missingInDom missing)
      (hash-set! rec 'extraInDom extra)
      (hash-set! rec 'note PAGE-NOTE)
      (when (and (pair? missing) (null? extra))
        (rep-warn! rep (format "turn ~a: page omits rollout(s) ~a that the API contains; the export keeps them"
                               i (py-list-repr missing)))))))

(define (check-summaries! rep t cap)
  (for ([turn (in-list (turns-of t))] #:when (assistant? turn))
    (define i (jget turn 'index))
    (define api (for/list ([p (in-list (api-events turn))] #:when (equal? (jget (cdr p) 'type) "summary"))
                  (norm (jget (cdr p) 'text))))
    (define pan (or-empty-hash (panel cap 'thoughtsByArticle i)))
    (define text (norm (jget pan 'innerText)))
    (define dom-rows* (for/list ([p (in-list (dom-rows pan))] #:when (equal? (jget (cdr p) 'type) "summary"))
                        (norm (jget (cdr p) 'text))))
    (define missing (for/list ([s api] #:when (and (truthy? s) (not (string-contains? text s)))) s))
    (define ok (and (null? missing) (= (length dom-rows*) (length api))))
    (rep-add! rep "summaries" i
              (hasheq 'count (length api) 'texts api)
              (hasheq 'count (length dom-rows*) 'texts dom-rows*)
              ok
              'missingFromPanelText missing)))

(define (check-chatroom! rep t cap)
  (for ([turn (in-list (turns-of t))] #:when (assistant? turn))
    (define i (jget turn 'index))
    (define msgs
      (for/list ([p (in-list (api-events turn))]
                 #:when (and (equal? (jget (cdr p) 'type) "tool") (equal? (jget (cdr p) 'kind) "chatroomSend")))
        (define m (jget (or-empty-hash (jget (cdr p) 'args)) 'message))
        (cons (jget (car p) 'id) (if (truthy? m) m ""))))
    (define pan (or-empty-hash (panel cap 'thoughtsByArticle i)))
    (define hay (string-append " " (string-join (md-words (jget pan 'innerText)) " ") " "))
    (define dom-msgs (for/list ([p (in-list (dom-rows pan))] #:when (equal? (jget (cdr p) 'type) "chatroom")) (cdr p)))
    (define api-word-lists '())
    (define results
      (for/list ([rm (in-list msgs)])
        (define w (md-words (cdr rm)))
        (define found (if (pair? w) (string-contains? hay (string-append " " (string-join w " ") " ")) #t))
        (set! api-word-lists (cons w api-word-lists))
        (hasheq 'rollout (car rm) 'words (length w) 'head (string-join (if (> (length w) 8) (take w 8) w) " ") 'found found)))
    (define dom-ok #t)
    (define dom-results
      (for/list ([r (in-list dom-msgs)])
        (define w (md-words (jget r 'message)))
        (define inapi (if (pair? w) (and (member w api-word-lists) #t) #t))
        (unless inapi (set! dom-ok #f))
        (hasheq 'words (length w) 'head (string-join (if (> (length w) 8) (take w 8) w) " ") 'inApi inapi)))
    (define not-found (filter (lambda (x) (not (jget x 'found))) results))
    (define ok (and dom-ok (<= (length dom-msgs) (length msgs))))
    (define rec (rep-add! rep "chatroom" i
                          (hasheq 'count (length msgs))
                          (hasheq 'count (length dom-msgs) 'messages results 'domMessages dom-results)
                          ok
                          'rule "every DOM chatroom message equals an API chatroomSend message; API messages absent from the page -> warning"))
    (when (or (pair? not-found) (not (= (length dom-msgs) (length msgs))))
      (hash-set! rec 'note PAGE-NOTE)
      (when ok
        (rep-warn! rep (format "turn ~a: page renders ~a of ~a chatroom messages; the export keeps all of them"
                               i (length dom-msgs) (length msgs)))))))

(define DOM-KIND
  (hash "webSearch" "Searched web" "xSearch" "Searched 𝕏" "xUserSearch" "Searched 𝕏" "browsePage" "Browsed"))
(define DOM-KIND-VALUES '("Searched web" "Searched 𝕏" "Browsed"))

(define (row-key r) (list (jget r 'rollout) (jget r 'kind) (jget r 'key)))

(define (key<? a b)
  (define (s v) (if (string? v) v (py-str v)))
  (let loop ([a a] [b b])
    (cond [(null? a) #f]
          [(string<? (s (car a)) (s (car b))) #t]
          [(string<? (s (car b)) (s (car a))) #f]
          [else (loop (cdr a) (cdr b))])))

(define (count-keys rows)
  (define h (make-hash))
  (for ([r rows]) (hash-update! h (row-key r) add1 0))
  h)

;; elements of (a - b) as a sorted list of keys (positive multiplicities repeated)
(define (counter-minus a b)
  (define out '())
  (for ([(k n) (in-hash a)])
    (define d (- n (hash-ref b k 0)))
    (for ([_ (in-range d)]) (set! out (cons k out))))
  (sort out key<?))

(define (check-tool-rows! rep t cap)
  (for ([turn (in-list (turns-of t))] #:when (assistant? turn))
    (define i (jget turn 'index))
    (define pan (panel cap 'sourcesByArticle i))
    (define exp-rows
      (filter
       (lambda (r) (not (and (equal? (hash-ref r 'apiKind) "browsePage") (string-prefix? (hash-ref r 'key) "grok.com/"))))   ; grok.com's own pages are not sources
      (for/list ([p (in-list (api-events turn))]
                 #:when (and (equal? (jget (cdr p) 'type) "tool")
                             (string? (jget (cdr p) 'kind))
                             (hash-has-key? DOM-KIND (jget (cdr p) 'kind))))
        (define ev (cdr p))
        (define kind (jget ev 'kind))
        (define args (or-empty-hash (jget ev 'args)))
        (define res (or-empty-hash (jget ev 'results)))
        (define items (or-empty-list (jget res 'items)))
        (define key (if (equal? kind "browsePage") (urlkey (jget args 'url)) (norm (jget args 'query))))
        (hasheq 'rollout (jget (car p) 'id) 'kind (hash-ref DOM-KIND kind) 'apiKind kind 'key key
                'count (if (equal? kind "webSearch") (length items) 'null)
                'urls (if (equal? kind "webSearch") (for/list ([x items]) (jget x 'url)) 'null)))))
    (unless (and (null? exp-rows) (not (truthy? pan)))
      (define ids (remove-duplicates
                   (for/list ([rl (in-list (or-empty-list (jget (or-empty-hash (jget turn 'thinking)) 'rollouts)))]
                              #:when (truthy? (jget rl 'events)))
                     (jget rl 'id))))
      (define sole (if (= (length ids) 1) (car ids) 'null))
      (define act-rows
        (for/list ([p (in-list (dom-rows (or-empty-hash pan)))]
                   #:when (member (jget (cdr p) 'kind) DOM-KIND-VALUES))
          (define r (cdr p))
          (define key (if (equal? (jget r 'kind) "Browsed")
                          (urlkey (py-or (jget r 'url) (jget r 'query)))
                          (norm (jget r 'query))))
          (hasheq 'rollout (let ([ro (jget (car p) 'rollout)]) (if (truthy? ro) ro sole)) 'kind (jget r 'kind) 'key key 'count (jget r 'count)
                  'urls (for/list ([x (or-empty-list (jget r 'results))]) (jget x 'url)))))
      (define exp-c (count-keys exp-rows))
      (define act-c (count-keys act-rows))
      (define missing (counter-minus exp-c act-c))
      (define extra (counter-minus act-c exp-c))
      (define used (make-hasheqv))
      (define act-vec (list->vector act-rows))
      (define url-mismatch
        (reverse
         (for/fold ([acc '()]) ([e (in-list exp-rows)] #:when (equal? (jget e 'kind) "Searched web"))
           (define ek (row-key e))
           (define k*
             (for/first ([k (in-range (vector-length act-vec))]
                         #:when (and (not (hash-ref used k #f))
                                     (equal? (row-key (vector-ref act-vec k)) ek)))
               k))
           (cond
             [(not k*) acc]
             [else
              (hash-set! used k* #t)
              (define a (vector-ref act-vec k*))
              (if (or (not (equal? (jget a 'count) (jget e 'count)))
                      (not (equal? (jget a 'urls) (jget e 'urls))))
                  (cons (hasheq 'rollout (jget e 'rollout) 'query (jget e 'key)
                                'apiCount (jget e 'count) 'domCount (jget a 'count)
                                'apiUrls (jget e 'urls) 'domUrls (jget a 'urls))
                        acc)
                  acc)]))))
      (define ok (and (null? missing) (null? extra) (null? url-mismatch)))
      (define (key-rec m) (hasheq 'rollout (car m) 'kind (cadr m) 'key (caddr m)))
      (rep-add! rep "tool-rows" i
                (hasheq 'rows (length exp-rows)
                        'webSearchRows (for/sum ([r exp-rows]) (if (equal? (jget r 'kind) "Searched web") 1 0))
                        'webResultUrls (for/sum ([r exp-rows]) (if (truthy? (jget r 'urls)) (length (jget r 'urls)) 0)))
                (hasheq 'rows (length act-rows)
                        'webSearchRows (for/sum ([r act-rows]) (if (equal? (jget r 'kind) "Searched web") 1 0))
                        'webResultUrls (for/sum ([r act-rows]) (if (equal? (jget r 'kind) "Searched web") (length (jget r 'urls)) 0))
                        'domRowsTotal (jget (or-empty-hash pan) 'rowCount))
                ok
                'missingInDom (map key-rec missing)
                'extraInDom (map key-rec extra)
                'urlMismatches url-mismatch))))

;; grok.com appends ?referrer=grok-com to X post links it renders
(define (href-key h) (if (string? h) (regexp-replace #px"[?&]referrer=grok-com$" h "") h))

(define (check-citations! rep t cap)
  (define arts (arts-of cap))
  (for ([turn (in-list (turns-of t))] #:when (assistant? turn))
    (define i (jget turn 'index))
    (define cits (or-empty-list (jget turn 'citations)))
    (define chips (or-empty-list (jget (art-at arts i) 'citationChips)))
    (unless (and (null? cits) (null? chips))
      (define api-urls (for/list ([c cits]) (jget c 'url)))
      (define have-urls (for/or ([u api-urls]) (truthy? u)))
      (define distinct
        (reverse (for/fold ([acc '()]) ([u api-urls]) (if (and (truthy? u) (not (member u acc))) (cons u acc) acc))))
      (define eff (for/sum ([c chips]) (chip-size c)))
      (define hrefs (for/list ([c chips] #:when (truthy? (jget c 'href))) (jget c 'href)))
      (define chip-hrefs (for/list ([c chips]) (jget c 'href)))
      (define chip-texts (for/list ([c chips]) (norm (jget c 'text))))
      (define exp (hasheq 'citations (length cits) 'distinctUrls (if have-urls (length distinct) 'null) 'urls api-urls))
      (define act (hasheq 'chips (length chips) 'effectiveChips eff 'chipTexts chip-texts 'chipHrefs chip-hrefs))
      (cond
        [have-urls
         (define count-ok (or (= eff (length distinct)) (= eff (length cits))))
         (define bad-href (for/list ([h hrefs] #:unless (member (href-key h) api-urls)) h))
         (define unverifiable (for/list ([u distinct] #:unless (member u (map href-key hrefs))) u))
         (define groups-without-href (for/list ([c chips] #:unless (truthy? (jget c 'href))) c))
         (define ok (and count-ok (null? bad-href) (or (null? unverifiable) (pair? groups-without-href))))
         (rep-add! rep "citations" i exp act ok
                   'hrefNotInApi bad-href 'apiUrlsWithoutHref unverifiable
                   'rule "effectiveChips == distinct API URLs, or == citations (one chip per citation); every href in API URLs")]
        [else
         (define ok (and (< 0 eff) (<= eff (length cits))))
         (define mode (if (= eff (length cits)) "one-to-one" "ambiguous"))
         (rep-add! rep "citations" i exp act ok
                   'rule "legacy transcript (no API URLs): 0 < effectiveChips <= citations; hrefs reported for enrichment"
                   'enrichment (hasheq 'mode mode 'chipHrefs chip-hrefs 'chipTexts chip-texts))
         (when (equal? mode "ambiguous")
           (rep-warn! rep (format "turn ~a: ~a citation(s) but ~a chip(s); citation URLs cannot be assigned from the DOM, left null"
                                  i (length cits) eff)))]))))

(define (sorted-int-keys d)
  (sort (for/list ([k (in-hash-keys d)]
                   #:when (exact-integer? (string->number (symbol->string k))))
          (string->number (symbol->string k)))
        <))

;; Searched images: the page's image figures link to the same pages as the API's image cards, in order.
(define (check-images! rep t cap)
  (define arts (arts-of cap))
  (for ([turn (in-list (turns-of t))] #:when (assistant? turn))
    (define i (jget turn 'index))
    (define api (or-empty-list (jget turn 'images)))
    (define dom (or-empty-list (jget (art-at arts i) 'images)))
    (unless (and (null? api) (null? dom))
      (define exp-links (for/list ([x api]) (jget x 'link)))
      (define act-links (for/list ([d dom]) (jget d 'link)))
      (rep-add! rep "images" i
                (hasheq 'count (length api) 'links exp-links)
                (hasheq 'count (length dom) 'links act-links 'srcs (for/list ([d dom]) (jget d 'src)))
                (equal? exp-links act-links)
                'rule "page image links equal the API image cards' links, in order"))))

(define (check-expansion! rep t cap)
  (for ([pair (in-list (list (cons 'thoughtsByArticle "thoughts") (cons 'sourcesByArticle "sources")))])
    (define d (or-empty-hash (jget cap (car pair))))
    (define name (cdr pair))
    (for ([k (in-list (sorted-int-keys d))])
      (define pan (hash-ref d (string->symbol (number->string k))))
      (unless (eq? pan 'null)
        (define rc (jget pan 'remainingCollapsed))
        (rep-add! rep "expansion-complete" k
                  (hasheq 'panel name 'remainingCollapsed 0)
                  (hasheq 'panel name 'remainingCollapsed rc 'rows (jget pan 'rowCount) 'links (jget pan 'linkCount))
                  (equal? rc 0))))))

(define (check-api-presence! rep t cap)
  (for ([turn (in-list (turns-of t))] #:when (assistant? turn))
    (define i (jget turn 'index))
    (define th (or-empty-hash (jget turn 'thinking)))
    (define has-api (for/or ([rl (in-list (or-empty-list (jget th 'rollouts)))]) (truthy? (jget rl 'events))))
    (define pan (panel cap 'thoughtsByArticle i))
    (when (and has-api (not (truthy? pan)))
      (rep-warn! rep (format "turn ~a: API has thinking events but the DOM capture has no Thoughts panel" i)))))

;; ------------------------------------------------------------------ verify (mode 1)
;; -> (values checks warnings) in report order
(define (run-checks t cap att-dir)
  (define rep (report '() '()))
  (check-turn-count! rep t cap)
  (check-user-text! rep t cap)
  (check-assistant-text! rep t cap)
  (check-attachments! rep t cap att-dir)
  (check-thought-label! rep t cap)
  (check-rollouts! rep t cap)
  (check-summaries! rep t cap)
  (check-chatroom! rep t cap)
  (check-tool-rows! rep t cap)
  (check-citations! rep t cap)
  (check-images! rep t cap)
  (check-expansion! rep t cap)
  (check-api-presence! rep t cap)
  (values (reverse (report-checks rep)) (reverse (report-warnings rep))))

;; Assemble the verification document from a list of checks (any origin) + warnings.
(define (assemble-result checks warnings #:tool tool #:extra [extra '()])
  (define failed (filter (lambda (c) (not (hash-ref c 'ok))) checks))
  (define by-name (make-hasheq))
  (for ([c checks])
    (define k (string->symbol (hash-ref c 'name)))
    (unless (hash-has-key? by-name k)
      (hash-set! by-name k (make-hasheq (list (cons 'total 0) (cons 'ok 0) (cons 'failed 0)))))
    (define b (hash-ref by-name k))
    (hash-update! b 'total add1)
    (hash-update! b (if (hash-ref c 'ok) 'ok 'failed) add1))
  (define base
    (hasheq 'tool tool
            'checks checks
            'byName by-name
            'summary (hasheq 'total (length checks) 'ok (- (length checks) (length failed)) 'failed (length failed))
            'failedChecks (for/list ([c failed]) (hasheq 'name (hash-ref c 'name) 'turnIndex (hash-ref c 'turnIndex)))
            'warnings warnings
            'ok (null? failed)))
  (for/fold ([h base]) ([kv (in-list extra)]) (hash-set h (car kv) (cdr kv))))

;; verify-capture: transcript jsexpr + capture jsexpr (+ optional attachments dir) -> result jsexpr
;; identical in shape to reference_verify.py's output (the `tool` field names this app).
(define (verify-capture t cap
                        #:attachments-dir [att-dir #f]
                        #:transcript-name [tname "transcript.json"]
                        #:capture-name [cname "dom-capture.json"]
                        #:tool [tool "grok-export-rkt verify"]
                        #:extra-checks [extra-checks '()]
                        #:extra-warnings [extra-warnings '()]
                        #:extra-fields [extra-fields '()])
  (define-values (checks warnings) (run-checks t cap att-dir))
  (assemble-result (append checks extra-checks) (append warnings extra-warnings)
                   #:tool tool
                   #:extra (append (list (cons 'transcript tname) (cons 'capture cname)) extra-fields)))

;; ------------------------------------------------------------------ stability (check 11)
(define (strip-env cap)
  (define out (make-hasheq))
  (for ([(k v) (in-hash (or-empty-hash cap))])
    (unless (memq k '(capturedAt env)) (hash-set! out k v)))
  (hash-set! out 'articles
             (for/list ([a (in-list (or-empty-list (jget cap 'articles)))])
               (for/hasheq ([(k v) (in-hash (or-empty-hash a))] #:unless (eq? k 'html))
                 ;; attachment names come from a hover tooltip that one read may render and the other not
                 (values k (if (and (eq? k 'attachments) (list? v))
                               (for/list ([x (in-list v)])
                                 (if (hash? x) (for/hasheq ([(kk vv) (in-hash x)] #:unless (eq? kk 'name)) (values kk vv)) x))
                               v)))))
  out)

(define (jtype v)
  (cond [(hash? v) 'dict] [(list? v) 'list] [(string? v) 'str] [(boolean? v) 'bool]
        [(eq? v 'null) 'none] [(exact-integer? v) 'int] [else 'other]))

(define (diff-paths a b [path ""] [limit 40])
  (define out '())
  (define n 0)
  (define (push! rec) (when (< n limit) (set! out (cons rec out)) (set! n (add1 n))))
  (define (s120 v) (str-head (py-str v) 120))
  (let walk ([a a] [b b] [path path])
    (when (< n limit)
      (cond
        [(not (eq? (jtype a) (jtype b)))
         (push! (hasheq 'path (if (string=? path "") "/" path) 'a (s120 a) 'b (s120 b)))]
        [(hash? a)
         (define keys (sort (remove-duplicates (append (map symbol->string (hash-keys a)) (map symbol->string (hash-keys b)))) string<?))
         (for ([k keys])
           (define ks (string->symbol k))
           (cond
             [(not (and (hash-has-key? a ks) (hash-has-key? b ks)))
              (push! (hasheq 'path (string-append path "/" k)
                             'a (if (hash-has-key? a ks) "present" "absent")
                             'b (if (hash-has-key? b ks) "present" "absent")))]
             [else (walk (hash-ref a ks) (hash-ref b ks) (string-append path "/" k))]))]
        [(list? a)
         (unless (= (length a) (length b))
           (push! (hasheq 'path path 'a (format "len ~a" (length a)) 'b (format "len ~a" (length b)))))
         (for ([x a] [y b] [k (in-naturals)])
           (walk x y (string-append path "/" (number->string k))))]
        [(not (equal? a b))
         (push! (hasheq 'path path 'a (s120 a) 'b (s120 b)))])))
  (reverse out))

;; cap1/cap2: capture jsexprs; name1/name2: file names for the record.
(define (stability-check cap1 cap2 name1 name2)
  (define s1 (jsexpr->canonical-string (strip-env cap1)))
  (define s2 (jsexpr->canonical-string (strip-env cap2)))
  (define ok (string=? s1 s2))
  (define rec (make-hasheq))
  (hash-set! rec 'name "dom-stability")
  (hash-set! rec 'turnIndex 'null)
  (hash-set! rec 'expected (hasheq 'sha256 (sha-str s1) 'bytes (bytes-length (string->bytes/utf-8 s1)) 'file name1))
  (hash-set! rec 'actual (hasheq 'sha256 (sha-str s2) 'bytes (bytes-length (string->bytes/utf-8 s2)) 'file name2))
  (hash-set! rec 'ok ok)
  (hash-set! rec 'ignored '("capturedAt" "env" "articles[*].html" "articles[*].attachments[*].name"))
  (hash-set! rec 'differences (if ok '() (diff-paths (strip-env cap1) (strip-env cap2))))
  rec)

;; ------------------------------------------------------------------ api-consistency (check 13)
(define CIT-PATH-RX #px"^/turns/[0-9]+/citations/[0-9]+/(url|kind)$")
(define API-CONSISTENCY-RULE
  (string-append "transcripts built from the chunk and the legacy payload differ only in citations[].url/kind "
                 "(X post results without an author, which one format lists and the other omits, are left out)"))

;; A post the API lists by id but never hydrates (deleted, unavailable) comes back as an X result with no
;; username in the chunk format and not at all in the legacy one.  -> copy of t without those items, each
;; turn's sources.toolResultRows lowered by the number dropped.
(define (drop-authorless-x t)
  (define (hset h k v) (hash-set (for/hasheq ([(a b) (in-hash h)]) (values a b)) k v))
  (define (fix-turn turn)
    (define removed 0)
    (define th (jget turn 'thinking))
    (cond
      [(not (hash? th)) turn]
      [else
       (define rollouts*
         (for/list ([rl (in-list (or-empty-list (jget th 'rollouts)))])
           (if (not (hash? rl))
               rl
               (hset rl 'events
                     (for/list ([ev (in-list (or-empty-list (jget rl 'events)))])
                       (define res (jget ev 'results))
                       (cond
                         [(and (hash? res) (equal? (jget res 'kind) "x"))
                          (define items (or-empty-list (jget res 'items)))
                          (define kept (filter (lambda (it) (truthy? (jget it 'username))) items))
                          (set! removed (+ removed (- (length items) (length kept))))
                          (hset ev 'results (hset res 'items kept))]
                         [else ev]))))))
       (cond
         [(zero? removed) turn]
         [else
          (define src (jget turn 'sources))
          (define turn1 (hset turn 'thinking (hset th 'rollouts rollouts*)))
          (if (and (hash? src) (exact-integer? (jget src 'toolResultRows)))
              (hset turn1 'sources (hset src 'toolResultRows (- (jget src 'toolResultRows) removed)))
              turn1)])]))
  (for/fold ([t* t]) ([key (in-list '(turns offBranchTurns))])
    (if (and (hash? t*) (list? (jget t* key)))
        (hset t* key (map fix-turn (jget t* key)))
        t*)))

;; t-chunk / t-legacy: transcript jsexprs built from the two share payloads.
(define (api-consistency-check t-chunk t-legacy #:chunk-name [n1 "chunk"] #:legacy-name [n2 "legacy"])
  (define rec (make-hasheq))
  (hash-set! rec 'name "api-consistency")
  (hash-set! rec 'turnIndex 'null)
  (hash-set! rec 'rule API-CONSISTENCY-RULE)
  (cond
    [(or (not (hash? t-chunk)) (not (hash? t-legacy)))
     (hash-set! rec 'expected (hasheq 'format n1 'available (hash? t-chunk)))
     (hash-set! rec 'actual (hasheq 'format n2 'available (hash? t-legacy)))
     (hash-set! rec 'ok #f)
     (hash-set! rec 'differences '())
     (hash-set! rec 'note "one of the two payloads was not available, the cross-check could not be performed")]
    [else
     (define s1 (jsexpr->canonical-string t-chunk))
     (define s2 (jsexpr->canonical-string t-legacy))
     (define diffs (diff-paths (drop-authorless-x t-chunk) (drop-authorless-x t-legacy) "" 100000))
     (define permitted (filter (lambda (d) (regexp-match? CIT-PATH-RX (hash-ref d 'path))) diffs))
     (define others (filter (lambda (d) (not (regexp-match? CIT-PATH-RX (hash-ref d 'path)))) diffs))
     (hash-set! rec 'expected (hasheq 'format n1 'sha256 (sha-str s1) 'bytes (bytes-length (string->bytes/utf-8 s1))))
     (hash-set! rec 'actual (hasheq 'format n2 'sha256 (sha-str s2) 'bytes (bytes-length (string->bytes/utf-8 s2))))
     (hash-set! rec 'ok (null? others))
     (hash-set! rec 'permittedDifferences (length permitted))
     (hash-set! rec 'differences (if (> (length others) 40) (take others 40) others))])
  rec)

;; ------------------------------------------------------------------ citation enrichment (DOM fallback)
;; t: transcript jsexpr; cap: DOM capture (round 1); chip-links: jsexpr
;; {"<articleIndex>": [{chip, text, size, links:[{href,text}]}]} produced by the group-chip helper
;; (revealed members of `button.inline` chips), or 'null.
;; SPEC 0.5: citations[].url/kind are resolved from the API (cardAttachmentsJson on the legacy
;; format, renderCitation chunks on the chunk format) by the transcript builder.  This pass is
;; strictly a FALLBACK for citations that are still null afterwards (a card id absent from
;; cardAttachmentsJson); citations that already carry a URL are never touched.
;; Rule (unchanged): a turn's chips are mapped only when the effective chip count (chips + their
;; "+N") equals the citation count (verifier mode "one-to-one").  Chips cover the citations in
;; offset order; a single a.citation chip gives its href to its citation; a group chip covers the
;; next `size` citations, and its revealed members (document order) are paired with those
;; citations sorted by citationId ascending (grok.com renders group members from a map keyed by
;; citation_id, whose integer keys iterate in ascending order).  kind = CITATION_KIND_WEB_PAGE
;; (the only kind the chunk API emits, also for x.com status URLs).  Anything else stays null.
;; -> (values transcript* notes)   notes: list of strings (for the manifest)
(define (enrich-citations t cap chip-links)
  (define arts (arts-of cap))
  (define notes '())
  (define (note! fmt . args) (set! notes (cons (apply format fmt args) notes)))
  (define turns*
    (for/list ([turn (in-list (turns-of t))])
      (define cits (or-empty-list (jget turn 'citations)))
      (define n-null (for/sum ([c cits]) (if (truthy? (jget c 'url)) 0 1)))
      (cond
        [(or (not (assistant? turn)) (null? cits)) turn]
        [(zero? n-null) turn]   ; every citation resolved from the API: nothing to do
        [else
         (define i (jget turn 'index))
         (define chips (or-empty-list (jget (art-at arts i) 'citationChips)))
         (define eff (for/sum ([c chips]) (chip-size c)))
         (cond
           [(not (= eff (length cits)))
            (note! "turn ~a: ~a of ~a citation(s) unresolved by the API, ~a effective chip(s) for ~a citations (ambiguous), left null"
                   i n-null (length cits) eff (length cits))
            turn]
           [else
            (define links-for-article
              (or-empty-list (jget (or-empty-hash chip-links) (string->symbol (number->string i)))))
            (define cit-vec (list->vector cits))
            (define assigned (make-hasheqv))   ; citation index -> url
            (define pos 0)
            (for ([chip (in-list chips)] [chip-k (in-naturals)])
              (define size (chip-size chip))
              (define covered (for/list ([k (in-range pos (+ pos size))]) k))
              (set! pos (+ pos size))
              (define href (jget chip 'href))
              (cond
                [(and (= size 1) (truthy? href))
                 (hash-set! assigned (car covered) href)]
                [(= size 1)
                 (note! "turn ~a: chip ~a (~s) has no href and no revealed members; citation ~a left null" i chip-k (norm (jget chip 'text)) (car covered))]
                [else
                 (define entry (for/first ([e links-for-article] #:when (equal? (jget e 'chip) chip-k)) e))
                 (define members (if entry (or-empty-list (jget entry 'links)) '()))
                 (cond
                   [(not (= (length members) size))
                    (note! "turn ~a: group chip ~a (~s) covers ~a citations but revealed ~a member link(s); left null"
                           i chip-k (norm (jget chip 'text)) size (length members))]
                   [else
                    (define by-id (sort covered < #:key (lambda (k) (let ([v (jget (vector-ref cit-vec k) 'citationId)]) (if (exact-integer? v) v 0)))))
                    (for ([k by-id] [m members])
                      (hash-set! assigned k (jget m 'href)))
                    (note! "turn ~a: group chip ~a (~s) expanded to ~a member link(s), paired with citations ~a by ascending citationId"
                           i chip-k (norm (jget chip 'text)) (length members) by-id)])]))
            ;; only citations the API left null receive a DOM value
            (define cits*
              (for/list ([c cits] [k (in-naturals)])
                (define u (hash-ref assigned k #f))
                (if (and u (string? u) (not (truthy? (jget c 'url))))
                    (let ([h (hash-copy c)]) (hash-set! h 'url u) (hash-set! h 'kind "CITATION_KIND_WEB_PAGE") h)
                    c)))
            (define n-filled (for/sum ([c cits] [c* cits*]) (if (and (not (truthy? (jget c 'url))) (truthy? (jget c* 'url))) 1 0)))
            (note! "turn ~a: ~a of ~a API-unresolved citation URL(s) filled from the DOM chips (fallback, kind CITATION_KIND_WEB_PAGE)" i n-filled n-null)
            (define turn* (hash-copy turn))
            (hash-set! turn* 'citations cits*)
            turn*])])))
  (define t* (hash-copy t))
  (hash-set! t* 'turns turns*)
  (values t* (reverse notes)))
