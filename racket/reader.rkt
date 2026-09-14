#lang racket/base
;; reader.rkt — the document model behind the in-app conversation reader.
;;
;; Turns a transcript.json (the exporter's own output) into a flat list of `block` records that
;; the canvas can measure, wrap and paint.  Nothing here touches racket/gui: it is pure data, so
;; it can be tested from the command line and reused by any front end.
(require racket/list
         racket/string
         racket/file
         racket/path
         json
         "analysis.rkt" "pairs.rkt" "record.rkt")

(provide (struct-out block)
         (struct-out export-entry)
         scan-exports
         load-conversation
         conversation-blocks
         tool-label
         fmt-duration
         fmt-when
         short-url
         strip-md)

;; ---------------------------------------------------------------- library
;; One exported conversation on disk.
(struct export-entry (dir title turns mode when checks warnings size) #:transparent)

(define (jref h k [d #f])
  (if (and (hash? h) (hash-has-key? h k)) (hash-ref h k) d))

(define (read-json-file p)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (and (file-exists? p) (call-with-input-file p read-json))))

(define (dir-size p)
  (with-handlers ([exn:fail? (lambda (e) 0)])
    (for/sum ([f (in-directory p)] #:when (file-exists? f)) (file-size f))))

(define (count-value v)
  (cond [(and (exact-integer? v) (>= v 0)) v]
        [(list? v) (length v)]
        [else #f]))

;; C manifests place the totals under verification.summary; Racket manifests
;; keep passed/total directly under verification.  A zero-total record means
;; verification was skipped, so the reader displays "--" instead of "0/0".
(define (verification-counts ver)
  (and (hash? ver)
       (let* ([summary (jref ver 'summary)]
              [passed (or (and (hash? summary)
                               (or (count-value (jref summary 'passed))
                                   (count-value (jref summary 'ok))))
                          (count-value (jref ver 'passed))
                          (and (not (boolean? (jref ver 'ok)))
                               (count-value (jref ver 'ok))))]
              [total (or (and (hash? summary) (count-value (jref summary 'total)))
                         (count-value (jref ver 'total)))])
         (and passed total (> total 0) (cons passed total)))))

(define (manifest-warning-count man ver)
  (define verified (and (hash? ver) (count-value (jref ver 'warnings))))
  (define top-level (and (hash? man) (count-value (jref man 'warnings))))
  (or verified top-level 0))

(define (conversation-key conv d)
  (define cid (jref conv 'conversationId))
  (define src (jref conv 'sourceUrl))
  (define title (jref conv 'title))
  (string-downcase
   (cond [(non-empty-string? cid) (string-append "id:" cid)]
         [(non-empty-string? src)
          (string-append "url:" (regexp-replace #px"[?#].*$" src ""))]
         [(non-empty-string? title) (string-append "title:" title)]
         [else (string-append "dir:" (path->string d))])))

;; Newest first.  A directory without a transcript.json is skipped (a cancelled or failed run).
(define (scan-exports root)
  (define dirs
    (with-handlers ([exn:fail? (lambda (e) '())])
      (if (directory-exists? root)
          (filter directory-exists?
                  (map (lambda (d) (build-path root d)) (directory-list root)))
          '())))
  (define keyed-entries
    (for*/list ([d (in-list dirs)]
                [kv (in-value (scan-entry/cached d))]
                #:when kv)
      kv))
  ;; Keep the newest complete export for each conversation.  Repeated verifier
  ;; and performance runs no longer flood the rail with the same transcript.
  (define sorted
    (sort keyed-entries string>?
          #:key (lambda (kv) (path->string (export-entry-dir (cdr kv))))))
  (define seen (make-hash))
  (for/list ([kv (in-list sorted)] #:unless (hash-ref seen (car kv) #f))
    (hash-set! seen (car kv) #t)
    (cdr kv)))

;; The rail is rescanned after every export.  Each scan read every transcript.json in full
;; (0.67 s for 54 folders); an entry is now re-read only when its transcript or manifest
;; changed on disk.
(define scan-cache (make-hash))
(define (file-stamp p)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (and (file-exists? p) (cons (file-or-directory-modify-seconds p) (file-size p)))))
(define (scan-entry/cached d)
  (define stamp (list (file-stamp (build-path d "transcript.json"))
                      (file-stamp (build-path d "manifest.json"))))
  (define hit (hash-ref scan-cache (path->string d) #f))
  (if (and hit (equal? (car hit) stamp) (car stamp))
      (cdr hit)
      (let ([kv (scan-entry d)])
        (hash-set! scan-cache (path->string d) (cons stamp kv))
        kv)))

;; (cons conversation-key export-entry), or #f for a directory without a transcript.json
(define (scan-entry d)
  (define t (read-json-file (build-path d "transcript.json")))
  (and
   (hash? t)
   (let ()
      (define conv (jref t 'conversation (hasheq)))
      (define turns (jref t 'turns '()))
      (define man (read-json-file (build-path d "manifest.json")))
      (define ver (and (hash? man) (jref man 'verification)))
      (define sandbox (and (hash? man) (jref man 'sandbox)))
      (define assistant (filter (lambda (x) (equal? (jref x 'sender) "assistant")) turns))
      (define mode
        (let ([m (and (pair? assistant) (jref (car assistant) 'model))]
              [sm (and (hash? sandbox) (jref sandbox 'mode))])
          (cond [(and (string? sm) (string-ci=? sm "build")) "Build"]
                [(and (string? m) (regexp-match? #rx"heavy" m)) "Heavy"]
                [(and (string? m) (regexp-match? #rx"build" m)) "Build"]
                [(string? m) m]
                [else "—"])))
      (cons (conversation-key conv d)
            (export-entry d
                          (or (jref conv 'title) (jref conv 'conversationId)
                              (path->string (file-name-from-path d)))
                          (length turns)
                          mode
                          (or (jref conv 'createTime) "")
                          (verification-counts ver)
                          (manifest-warning-count man ver)
                          (dir-size d))))))

(define (load-conversation dir)
  (define transcript (read-json-file (build-path dir "transcript.json")))
  (define behavior (read-json-file (build-path dir "behavior-report.json")))
  (if (hash? transcript)
      (let ([with-report (if (hash? behavior)
                             (hash-set transcript '_behaviorReport behavior)
                             transcript)])
        (if (file-exists? (build-path dir "behavior-report.md"))
            (hash-set with-report '_behaviorReportPath
                      (path->string (build-path dir "behavior-report.md")))
            with-report))
      transcript))

;; ---------------------------------------------------------------- blocks
;; kind        : 'meta 'user 'reply 'note 'group 'summary 'tool 'result 'source 'code 'image 'rule
;; text        : the string to draw (already plain)
;; indent      : nesting depth, 0..3
;; key         : #f, or a unique symbol/string used to remember expand state
;; open?       : for a group, whether it starts expanded
;; parent      : key of the enclosing group, or #f
;; extra       : kind-specific payload (url, path, count, colour hint)
(struct block (kind text indent key open? parent extra) #:transparent)

(define (tool-label k)
  (case k
    [("webSearch") "Searched web"] [("xSearch") "Searched X"] [("xUserSearch") "Searched X users"]
    [("browsePage") "Browsed"] [("conversationSearch") "Searched conversations"]
    [("viewImage") "Viewed image"] [("initTerminalSession") "Connected to computer"]
    [("chatroomSend") "Sent to the team"]
    [("bash") "Ran command"] [("editFile") "Wrote file"] [("readFile") "Read file"]
    [("listDir") "Listed directory"] [("imageSearch") "Searched images"] [("mcp") "Tool"]
    [else (or k "Tool")]))

(define (fmt-duration ms)
  (cond [(not (number? ms)) ""]
        [(< ms 1000) (format "~a ms" ms)]
        [(< ms 60000) (format "~a s" (quotient (+ ms 500) 1000))]
        [else (let* ([s (quotient (+ ms 500) 1000)]) (format "~am ~as" (quotient s 60) (remainder s 60)))]))

(define MONTHS #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
(define (fmt-when iso)
  (define m (and (string? iso) (regexp-match #px"^(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2})" iso)))
  (if m
      (let ([y (list-ref m 1)] [mo (string->number (list-ref m 2))] [d (list-ref m 3)]
            [hh (list-ref m 4)] [mm (list-ref m 5)])
        (format "~a ~a ~a · ~a:~a" (string->number d) (vector-ref MONTHS (sub1 mo)) y hh mm))
      (or iso "")))

(define (short-url u)
  (define s (or u ""))
  (define m (regexp-match #px"^https?://(?:www\\.)?([^/]+)(/.*)?$" s))
  (if m (string-append (cadr m) (let ([p (caddr m)]) (if (and p (> (string-length p) 1)) p ""))) s))

(define (strip-md s)
  ;; The reader draws prose, so the markdown emphasis markers themselves are noise.
  ;; Keeps the text, drops the ** * ` and [..](..) syntax around it.
  (let* ([s (or s "")]
         [s (regexp-replace* #px"[[]([^]]*)[]][(]([^)]*)[)]" s "\\1")]
         [s (regexp-replace* #px"[*][*]([^*]+)[*][*]" s "\\1")]
         [s (regexp-replace* #px"(^|[^*])[*]([^*]+)[*]" s "\\1\\2")]
         [s (regexp-replace* #px"`([^`]+)`" s "\\1")]
         [s (regexp-replace* #px"  +" s " ")])
    (string-trim s)))

(define (clean s) (string-trim (regexp-replace* #px"\\s+" (or s "") " ")))

;; Build the flat block list for one conversation.
(define (conversation-blocks t)
  (define conv (jref t 'conversation (hasheq)))
  (define turns (jref t 'turns '()))
  (define out '())
  (define (emit! k text indent [key #f] [open? #f] [parent #f] [extra #f])
    (set! out (cons (block k text indent key open? parent extra) out)))

  ;; header ------------------------------------------------------------
  (define assistant (filter (lambda (x) (equal? (jref x 'sender) "assistant")) turns))
  (define agents
    (let ([th (and (pair? assistant) (jref (car assistant) 'thinking))])
      (if (hash? th) (length (jref th 'rollouts '())) 0)))
  (emit! 'title (or (jref conv 'title) "Conversation") 0)
  (emit! 'meta
         (string-join
          (filter values
                  (list (format "~a turns" (length turns))
                        (and (pair? assistant)
                             (let ([m (jref (car assistant) 'model)])
                               (cond [(and (string? m) (regexp-match? #rx"heavy" m)) "Heavy"]
                                     [(and (string? m) (regexp-match? #rx"build" m)) "Build"]
                                     [(string? m) m] [else #f])))
                        (and (> agents 0) (format "~a agents" agents))
                        (let ([w (fmt-when (jref conv 'createTime))]) (and (non-empty-string? w) w))))
          "  ·  ") 0)
  (emit! 'rule "" 0)

  ;; The handler's own read across the whole conversation, stated before the transcript.
  ;; A single flagged reply is easy to wave off as a one-off; the tally is what makes a
  ;; habit visible, so the moves are counted and the reply numbers listed beside them.
  ;; Computed once: the dossier tallies these and each reply shows its own share below it.
  (define record-findings (analyse-record t))
  (define findings-by-turn (make-hash))
  (define handler-findings
    (let loop ([ts turns] [prev ""] [acc '()])
      (cond
        [(null? ts) (reverse acc)]
        [else
         (define turn (car ts))
         (define body (or (jref turn 'text) ""))
         (define idx (jref turn 'index 0))
         (cond
           [(equal? (jref turn 'sender) "human") (loop (cdr ts) body acc)]
           [else
            (define fs (sort (append (analyse-text idx body)
                                     (analyse-pair idx prev body)
                                     (hash-ref record-findings idx '()))
                             < #:key finding-start))
            (hash-set! findings-by-turn idx fs)
            (loop (cdr ts) prev (append (reverse fs) acc))])])))

  (unless (null? handler-findings)
    (define groups (make-hash))
    (for ([f (in-list handler-findings)])
      (hash-update! groups (finding-name f) (lambda (l) (cons f l)) '()))
    (define rows
      (sort (hash->list groups)
            (lambda (a b)
              (let ([na (length (cdr a))] [nb (length (cdr b))])
                (if (= na nb) (string<? (car a) (car b)) (> na nb))))))
    (define touched (length (remove-duplicates (map finding-turn handler-findings))))
    (define dk "handler-dossier")
    (emit! 'dossier
           (format "~a move~a across ~a repl~a"
                   (length handler-findings) (if (= 1 (length handler-findings)) "" "s")
                   touched (if (= 1 touched) "y" "ies"))
           0 dk #t #f
           (format "~a distinct" (length rows)))
    (for ([kv (in-list rows)])
      (define fs (reverse (cdr kv)))
      (define sev (apply max (map finding-sev fs)))
      (define ns (remove-duplicates (map finding-turn fs)))
      (define shown (if (> (length ns) 10) (take ns 10) ns))
      (emit! 'tally (car kv) 1 #f #f dk
             (list (length fs) sev (finding-cat (car fs))
                   (string-append
                    (string-join (map number->string shown) ", ")
                    (if (> (length ns) 10) (format ", +~a more" (- (length ns) 10)) "")))))
    (emit! 'rule "" 0))

  ;; The PDF-derived detector is a separate audited artifact from analysis.rkt's
  ;; HANDLER regexes.  Keep its contract, counts, and evidence visually distinct.
  (define behavior (jref t '_behaviorReport))
  (when (hash? behavior)
    (define findings (jref behavior 'findings '()))
    (define summary (jref behavior 'summary (hasheq)))
    (define confirmed (or (jref summary 'confirmed) 0))
    (define candidate (or (jref summary 'candidate) 0))
    (define not-assessable (or (jref summary 'notAssessable) 0))
    (define bk "pdf-behavior-report")
    (emit! 'behaviorgroup
           (format "~a finding~a" (length findings) (if (= (length findings) 1) "" "s"))
           0 bk (> (length findings) 0) #f
           (list confirmed candidate not-assessable (jref t '_behaviorReportPath)))
    (for ([f (in-list findings)])
      (define after (jref f 'after (hasheq)))
      (define before (jref f 'before))
      (define evidence
        (cond [(and (hash? after) (non-empty-string? (jref after 'quote))) (jref after 'quote)]
              [(and (hash? before) (non-empty-string? (jref before 'quote))) (jref before 'quote)]
              [else "No quoted evidence was exported for this finding."]))
      (define category (or (jref f 'category) "uncategorized"))
      (define label (string-titlecase (string-replace category "_" " ")))
      (emit! 'behaviorfinding label 1 #f #f bk
             (list category
                   (or (jref f 'status) "not_assessable")
                   (or (jref f 'severity) "unknown")
                   evidence
                   (or (jref f 'explanation) "")
                   (format "~a · subject turn ~a · ~a"
                           (or (jref f 'id) "finding")
                           (or (jref f 'recordedSubjectTurnIndex)
                               (jref f 'subjectTurnIndex)
                               "?")
                           (or (jref f 'proposition) "")))))
    (emit! 'rule "" 0))

  ;; turns --------------------------------------------------------------
  (for ([turn (in-list turns)])
    (define idx (jref turn 'index 0))
    (define sender (jref turn 'sender))
    (cond
      ;; ---- the user's message
      [(equal? sender "human")
       (for ([a (in-list (jref turn 'attachments '()))])
         (emit! 'image (or (jref a 'fileName) "attachment") 0 #f #f #f
                (list (jref a 'fileId) (jref a 'sizeBytes) (jref a 'mimeType))))
       (for ([para (in-list (string-split (or (jref turn 'text) "") "\n"))])
         (unless (string=? (string-trim para) "")
           (emit! 'user para 0)))]
      ;; ---- Grok's turn: thinking, then the reply, then sources
      [else
       (define th (jref turn 'thinking))
       (define rollouts (if (hash? th) (jref th 'rollouts '()) '()))
       (define n-events (for/sum ([r (in-list rollouts)]) (length (jref r 'events '()))))
       (when (> n-events 0)
         (define gk (format "think~a" idx))
         (emit! 'group
                (format "Thought for ~a" (fmt-duration (and (hash? th) (jref th 'durationMs))))
                0 gk #f #f
                (format "~a agent~a · ~a step~a"
                        (length rollouts) (if (= 1 (length rollouts)) "" "s")
                        n-events (if (= 1 n-events) "" "s")))
         (for ([r (in-list rollouts)])
           (define evs (jref r 'events '()))
           (unless (null? evs)
             (define rk (format "~a/~a" gk (jref r 'id)))
             (emit! 'group
                    (string-append (or (jref r 'id) "Grok")
                                   (let ([role (jref r 'role)]) (if (equal? role "Leader") "  Leader" "")))
                    1 rk #t gk
                    (format "~a step~a" (length evs) (if (= 1 (length evs)) "" "s")))
             (for ([e (in-list evs)] [i (in-naturals)])
               (define type (jref e 'type))
               (cond
                 [(equal? type "summary") (emit! 'summary (clean (jref e 'text)) 2 #f #f rk)]
                 [(equal? type "tool")
                  (define kind (jref e 'kind))
                  (define args (jref e 'args (hasheq)))
                  (define res (jref e 'results))
                  (define items (if (hash? res) (jref res 'items '()) '()))
                  (define ek (format "~a/e~a" rk i))
                  (define headline
                    (case kind
                      [("chatroomSend") ""]
                      [("bash") (clean (jref args 'description))]
                      [("editFile") (or (jref args 'filePath) "")]
                      [("readFile") (or (jref args 'filePath) "")]
                      [("listDir") (or (jref args 'targetDirectory) "")]
                      [("imageSearch") (clean (jref args 'imageDescription))]
                      [("mcp") (or (jref args 'toolName) "")]
                      [("browsePage") (short-url (jref args 'url))]
                      [("initTerminalSession") (short-url (jref args 'previewUrl))]
                      [else (clean (jref args 'query))]))
                  (define payload
                    (case kind
                      [("chatroomSend") (or (jref args 'message) "")]
                      [("bash") (or (jref args 'command) "")]
                      [("editFile") (or (jref args 'newString) "")]
                      [("mcp") (or (jref args 'toolArgsJson) "")]
                      [else ""]))
                  (define has-body? (or (non-empty-string? payload) (pair? items)))
                  (cond
                    [has-body?
                     (emit! 'group (tool-label kind) 2 ek #f rk
                            (list headline (if (pair? items) (format "~a result~a" (length items)
                                                                     (if (= 1 (length items)) "" "s")) "")))
                     (when (non-empty-string? payload)
                       (emit! 'code payload 3 #f #f ek
                              (if (member kind '("bash" "editFile" "mcp")) 'mono 'prose)))
                     (for ([it (in-list items)])
                       (cond
                         [(jref it 'url)
                          (emit! 'result (or (jref it 'title) (short-url (jref it 'url))) 3 #f #f ek
                                 (jref it 'url))]
                         [else
                          (emit! 'result (format "@~a  ~a" (or (jref it 'username) "?")
                                                 (clean (jref it 'text)))
                                 3 #f #f ek
                                 (let ([pid (jref it 'postId)])
                                   (and pid (format "https://x.com/i/status/~a" pid))))]))]
                    [else
                     (emit! 'tool (tool-label kind) 2 #f #f rk headline)])]
                 [else (emit! 'summary (clean (or (jref e 'text) type)) 2 #f #f rk)])))))
       ;; the public reply - markdown-ish lines become their own kinds so the reader draws a
       ;; bullet, a heading or a quote instead of printing the marker characters.
       (for ([para (in-list (string-split (or (jref turn (quote text)) "") (string (integer->char 10))))])
         (define s (string-trim para))
         (cond
           [(string=? s "") (void)]
           [(regexp-match #px"^#{1,6}[[:space:]]+(.*)$" s) => (lambda (m) (emit! (quote head) (strip-md (cadr m)) 0))]
           [(regexp-match #px"^[-*+][[:space:]]+(.*)$" s) => (lambda (m) (emit! (quote bullet) (strip-md (cadr m)) 0))]
           [(regexp-match #px"^([0-9]+)[.)][[:space:]]+(.*)$" s)
            => (lambda (m) (emit! (quote bullet) (strip-md (caddr m)) 0 #f #f #f (cadr m)))]
           [(regexp-match #px"^>[[:space:]]?(.*)$" s) => (lambda (m) (emit! (quote quote) (strip-md (cadr m)) 0))]
           [else (emit! (quote reply) (strip-md s) 0)]))
       ;; citations under the reply
       (let ([cits (jref turn 'citations '())])
         (unless (null? cits)
           (define ck (format "cite~a" idx))
           (emit! 'group "Citations" 0 ck #f #f (format "~a" (length cits)))
           (for ([c (in-list cits)])
             (emit! 'result (short-url (jref c 'url)) 1 #f #f ck (jref c 'url)))))
       ;; handler's read: what the machine is doing rhetorically in this reply
       (let* ([fs (hash-ref findings-by-turn idx '())]
              [n (length fs)])
         (when (> n 0)
           (define fk (format "flags~a" idx))
           (define highs (for/sum ([f fs]) (if (= 3 (finding-sev f)) 1 0)))
           (emit! (quote flaggroup)
                  (format "~a rhetorical finding~a" n (if (= n 1) "" "s"))
                  0 fk #f #f
                  (if (> highs 0) (format "~a HIGH" highs) ""))
           (for ([f (in-list fs)])
             (emit! (quote finding)
                    (finding-name f) 1 #f #f fk
                    (list (finding-cat f) (finding-sev f) (finding-quote f)
                          (finding-note f) (finding-ref f))))))
       ;; sources
       (let* ([s (jref turn 'sources)]
              [web (if (hash? s) (jref s 'webSearchResults '()) '())]
              [xs (if (hash? s) (jref s 'xposts '()) '())]
              [n (+ (length web) (length xs))])
         (when (> n 0)
           (define sk (format "src~a" idx))
           (emit! 'group "Sources" 0 sk #f #f (format "~a" n))
           (for ([w (in-list web)])
             (emit! 'source (or (jref w 'title) (short-url (jref w 'url))) 1 #f #f sk (jref w 'url)))
           (for ([p (in-list xs)])
             (emit! 'source (format "@~a  ~a" (or (jref p 'username) "?") (clean (jref p 'text))) 1 #f #f sk
                    (let ([pid (jref p 'postId)]) (and pid (format "https://x.com/i/status/~a" pid)))))))
       (emit! 'rule "" 0)]))
  (reverse out))
