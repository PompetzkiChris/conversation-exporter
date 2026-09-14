#lang racket/base
;; grok-export.rkt — Grok Heavy conversation exporter (Racket application).
;;
;; Implements SPEC.md sections 1-7: CLI, CDP flow (navigate, wait, API lane, DOM lane with
;; --rounds captures + screenshots, attachments), the section 5 verifier (verify.rkt),
;; transcript.json/.md/.html, verification.json, manifest.json and the exit codes.
(require racket/string
         racket/list
         racket/file
         json
         "util.rkt"
         "jsonw.rkt"
         "transcript.rkt"
         "md.rkt"
         "html.rkt"
         "http.rkt"
         "cdp.rkt"
         "launch.rkt"
         "hydrate.rkt"
         "verify.rkt"
         (prefix-in behavior: "behavior.rkt")
         "extract-js.rkt"
         "gemini.rkt"
         "qwen.rkt"
         racket/system)

(provide main
         normalize-url
         classify-url
         DEFAULT-OUT
         TOOL-NAME
         TOOL-VERSION)

(define TOOL-NAME "grok-export-rkt")
(define TOOL-VERSION "0.2.0")
;; the author's machine keeps exports in the project tree; everyone else gets Documents\Exporter\exports
(define DEFAULT-OUT
  (if (directory-exists? "C:\\ClaudeOutput\\grok-export-claude\\exports")
      "C:\\ClaudeOutput\\grok-export-claude\\exports"
      (path->string (build-path (find-system-path 'doc-dir) "Exporter" "exports"))))

;; ------------------------------------------------------------------ embedded extractor self-check
(define (check-embedded-js!)
  (define bs (string->bytes/utf-8 extract-js))
  (when (regexp-match? #rx"(?i:placeholder)" extract-js)
    (error 'startup "the embedded extract.js is a placeholder (contains the word \"placeholder\"); run python shared/embed.py and rebuild"))
  (unless (regexp-match? #rx"^\\(async \\(opts\\) => \\{" extract-js)
    (error 'startup "the embedded extract.js does not start with '(async (opts) => {'"))
  (unless (= (bytes-length bs) extract-js-size)
    (error 'startup "embedded extract.js size ~a != recorded size ~a" (bytes-length bs) extract-js-size))
  (unless (string=? (sha256-hex bs) extract-js-sha256)
    (error 'startup "embedded extract.js sha256 ~a != recorded ~a" (sha256-hex bs) extract-js-sha256)))

;; ------------------------------------------------------------------ command line
(define (usage)
  (for-each
   displayln
   (list
    "usage: grok-export <url-or-conversation-id> [--out DIR] [--port N] [--chrome PATH] [--profile DIR]"
    "                   [--no-launch] [--keep-open] [--timeout SEC] [--skip-dom] [--skip-api] [--rounds N] [--verbose]"
    "       grok-export --from-json FILE --source-url URL --out DIR [--deployment-json RESPONSE.json]"
    "                                                               (developer mode: parse + write only)"
    "       grok-export --verify-transcript transcript.json --verify-capture dom-capture.json [--verify-capture round2.json ...]"
    "                   [--attachments DIR] --out verification.json   (developer mode: verifier only)"
    ""
    (format "defaults: --out ~a  --port 9222  --timeout 600  --rounds 2" DEFAULT-OUT)
    (format "          --chrome ~a" (default-chrome-path))
    (format "          --profile ~a" (default-profile-dir))
    "exit codes: 0 exported and verified, 2 exported but verification found mismatches (see verification.json), 1 fatal error")))

(define (parse-args argv)
  (define o (make-hasheq))
  (hash-set! o 'out DEFAULT-OUT)
  (hash-set! o 'port 9222)
  (hash-set! o 'chrome (default-chrome-path))
  (hash-set! o 'profile (default-profile-dir))
  (hash-set! o 'no-launch #f)
  (hash-set! o 'keep-open #f)
  (hash-set! o 'timeout 600)
  (hash-set! o 'skip-dom #f)
  (hash-set! o 'skip-api #f)
  (hash-set! o 'rounds 2)
  (hash-set! o 'verbose #f)
  (hash-set! o 'from-json #f)
  (hash-set! o 'source-url #f)
  (hash-set! o 'deployment-json #f)
  (hash-set! o 'verify-transcript #f)
  (hash-set! o 'verify-captures '())
  (hash-set! o 'attachments #f)
  (hash-set! o 'url #f)
  (hash-set! o 'help #f)
  (define (need-value flag rest)
    (when (null? rest) (error 'args "~a needs a value" flag))
    (car rest))
  (define (need-int flag rest)
    (define v (string->number (need-value flag rest)))
    (unless (exact-integer? v) (error 'args "~a needs an integer, got ~a" flag (car rest)))
    v)
  (let loop ([args (vector->list argv)])
    (unless (null? args)
      (define a (car args))
      (define rest (cdr args))
      (cond
        [(member a '("--help" "-h" "/?")) (hash-set! o 'help #t) (loop rest)]
        [(string=? a "--out") (hash-set! o 'out (need-value a rest)) (loop (cdr rest))]
        [(string=? a "--port") (hash-set! o 'port (need-int a rest)) (loop (cdr rest))]
        [(string=? a "--chrome") (hash-set! o 'chrome (need-value a rest)) (loop (cdr rest))]
        [(string=? a "--profile") (hash-set! o 'profile (need-value a rest)) (loop (cdr rest))]
        [(string=? a "--timeout") (hash-set! o 'timeout (need-int a rest)) (loop (cdr rest))]
        [(string=? a "--rounds") (hash-set! o 'rounds (need-int a rest)) (loop (cdr rest))]
        [(string=? a "--from-json") (hash-set! o 'from-json (need-value a rest)) (loop (cdr rest))]
        [(string=? a "--source-url") (hash-set! o 'source-url (need-value a rest)) (loop (cdr rest))]
        [(string=? a "--deployment-json") (hash-set! o 'deployment-json (need-value a rest)) (loop (cdr rest))]
        [(string=? a "--verify-transcript") (hash-set! o 'verify-transcript (need-value a rest)) (loop (cdr rest))]
        [(string=? a "--verify-capture")
         (hash-set! o 'verify-captures (append (hash-ref o 'verify-captures) (list (need-value a rest))))
         (loop (cdr rest))]
        [(string=? a "--attachments") (hash-set! o 'attachments (need-value a rest)) (loop (cdr rest))]
        [(string=? a "--no-launch") (hash-set! o 'no-launch #t) (loop rest)]
        [(string=? a "--keep-open") (hash-set! o 'keep-open #t) (loop rest)]
        [(string=? a "--skip-dom") (hash-set! o 'skip-dom #t) (loop rest)]
        [(string=? a "--no-attachments") (hash-set! o 'no-attachments #t) (loop rest)]
        [(string=? a "--skip-api") (hash-set! o 'skip-api #t) (loop rest)]
        [(string=? a "--verbose") (hash-set! o 'verbose #t) (loop rest)]
        [(and (> (string-length a) 2) (string=? (substring a 0 2) "--"))
         (error 'args "unknown option ~a" a)]
        [(hash-ref o 'url) (error 'args "unexpected extra argument ~a" a)]
        [else (hash-set! o 'url a) (loop rest)])))
  (when (< (hash-ref o 'rounds) 1) (error 'args "--rounds must be >= 1"))
  o)

;; ------------------------------------------------------------------ URLs
(define UUID-RX #px"^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$")

(define (normalize-url s)
  (cond
    [(regexp-match? UUID-RX s) (string-append "https://grok.com/c/" s)]
    [(regexp-match? #px"^https?://" s) s]
    [else (error 'args "not a URL or conversation id: ~a" s)]))

;; -> (values kind id) with kind in 'share 'conversation 'unknown
(define (classify-url u)
  (cond
    [(regexp-match #px"^https?://[^/]+/share/([^/?#]+)" u)
     => (lambda (m) (values 'share (cadr m)))]
    [(regexp-match #px"^https?://[^/]+/c/([^/?#]+)" u)
     => (lambda (m) (values 'conversation (cadr m)))]
    [else (values 'unknown #f)]))

(define (int-or v default) (if (exact-integer? v) v default))

;; Progress for an embedding host (the GUI).  `report!` is a no-op unless a report-hook is
;; installed, so the console application is unaffected.
(define (phase! label frac) (report! 'phase (list label frac)))

;; ------------------------------------------------------------------ run state
(struct run (opts
             started-at
             t0
             [export-dir #:mutable]
             [files #:mutable]       ; list of (hasheq path sizeBytes sha256)
             [warnings #:mutable]
             [timings #:mutable]     ; alist name -> ms
             [raws #:mutable]        ; list of (hasheq name url status bytes file)
             [pending #:mutable]     ; raw payloads not yet written: (list rel bytes)
             [counts #:mutable]
             [api-lane #:mutable]
             [parsed-format #:mutable]
             [final-url #:mutable]
             [conversation-id #:mutable]
             [transport #:mutable]
             [dom-rounds #:mutable]        ; list of (hasheq round file capture ...) in order
             [chip-links #:mutable]        ; jsexpr from the group-chip helper (round 1) or 'null
             [screenshots #:mutable]       ; list of relative paths
             [verification #:mutable]      ; jsexpr or 'null
             [behavior #:mutable]          ; PDF-derived behavior report, or 'null
             [behavior-error #:mutable]    ; diagnostic string, or #f
             [behavior-ms #:mutable]
             [behavior-status #:mutable]   ; "ok" | "error" | "not_run"
             [enrichment #:mutable]        ; list of note strings
             [citation-sources #:mutable]  ; jsexpr: which source filled each citation, or 'null
             [attachment-paths #:mutable]  ; hash fileId -> relative path
             [notes #:mutable]            ; free-form manifest notes
             [sandbox #:mutable]))        ; SPEC 0.6 item 3: sandbox previewUrls + deployment

(define (make-run opts)
  (run opts (iso-now-utc) (now-ms) #f '() '() '() '() '() (hasheq) "none" 'null 'null 'null 'null
       '() 'null '() 'null 'null #f 0 "not_run" '() 'null (hash) '() 'null))

(define (warn! R fmt . args)
  (define msg (apply format fmt args))
  (log-warn "~a" msg)
  (set-run-warnings! R (append (run-warnings R) (list msg))))

(define (note! R fmt . args)
  (define msg (apply format fmt args))
  (log-info "~a" msg)
  (set-run-notes! R (append (run-notes R) (list msg))))

(define (timing! R name t-start)
  (set-run-timings! R (append (run-timings R) (list (cons name (- (now-ms) t-start))))))

;; Write bytes under the export dir (relative path with forward slashes) and record them;
;; a path written twice keeps one entry (the latest bytes).
(define (emit! R rel bs)
  (define dir (run-export-dir R))
  (unless dir (error 'emit "export directory not created yet (~a)" rel))
  (define full (build-path dir rel))
  (write-file-bytes full bs)
  (define entry (hasheq 'path rel 'sizeBytes (bytes-length bs) 'sha256 (sha256-hex bs)))
  (set-run-files! R (append (filter (lambda (f) (not (equal? (hash-ref f 'path) rel))) (run-files R))
                            (list entry)))
  full)

(define (emit-string! R rel s) (emit! R rel (string->bytes/utf-8 s)))

;; Queue a raw payload; written as soon as the export directory exists.
(define (queue-raw! R rel bs)
  (if (run-export-dir R)
      (emit! R rel bs)
      (set-run-pending! R (append (run-pending R) (list (list rel bs))))))

(define (flush-pending! R)
  (for ([p (in-list (run-pending R))])
    (emit! R (car p) (cadr p)))
  (set-run-pending! R '()))

(define (make-export-dir! R id)
  (define root (simplify-path (path->complete-path (hash-ref (run-opts R) 'out))))
  (ensure-dir! root)
  (define base (format "~a-~a" (dir-timestamp-local) (safe-file-name (if (string? id) id "unknown"))))
  (define dir
    (let loop ([n 0])
      (define name (if (zero? n) base (format "~a-~a" base (add1 n))))
      (define p (build-path root name))
      (if (or (directory-exists? p) (file-exists? p))
          (loop (add1 n))
          (begin (make-directory* p) p))))
  (set-run-export-dir! R dir)
  (report! 'export-dir (path->string dir))
  (log-info "Export directory: ~a" (path->string dir))
  (flush-pending! R)
  dir)


;; ------------------------------------------------------------------ sandbox (SPEC 0.6 item 3)
;; A Build (Beta) conversation runs in a live grok-sandbox.com container: every
;; initTerminalSession tool card carries that sandbox's previewUrl (the same host repeats until
;; the session is recycled), and grok.com may hold an app deployment for the conversation.
;; manifest.json records both, plus the mode the conversation ran in
;; (metadata.request_metadata.model: "build" for Build, "heavy" for Heavy).  A Heavy export gets
;; the same object with an empty previewUrls list.

;; distinct non-empty previewUrls over every tool card, in first-appearance order.
(define (sandbox-preview-urls t)
  (define seen (make-hash))
  (define acc '())
  (for* ([turn (in-list (or-empty-list (jget t 'turns)))]
         [rl (in-list (or-empty-list (jget (or-empty-hash (jget turn 'thinking)) 'rollouts)))]
         [ev (in-list (or-empty-list (jget rl 'events)))]
         #:when (equal? (jget ev 'type) "tool"))
    (define a (or-empty-hash (jget ev 'args)))
    (define u (py-or (jget a 'previewUrl) (jget a 'preview_url) 'null))
    (when (and (string? u) (> (string-length u) 0) (not (hash-has-key? seen u)))
      (hash-set! seen u #t)
      (set! acc (cons u acc))))
  (reverse acc))

(define (sandbox-terminal-sessions t)
  (for*/sum ([turn (in-list (or-empty-list (jget t 'turns)))]
             [rl (in-list (or-empty-list (jget (or-empty-hash (jget turn 'thinking)) 'rollouts)))]
             [ev (in-list (or-empty-list (jget rl 'events)))])
    (if (and (equal? (jget ev 'type) "tool") (equal? (jget ev 'kind) "initTerminalSession")) 1 0)))

;; distinct non-empty values of the given getters over the raw responses, first-appearance order.
(define (distinct-strings getters d)
  (define seen (make-hash))
  (define acc '())
  (for* ([r (in-list (or-empty-list (jget d 'responses)))]
         [g (in-list getters)])
    (define v (g r))
    (when (and (string? v) (> (string-length v) 0) (not (hash-has-key? seen v)))
      (hash-set! seen v #t)
      (set! acc (cons v acc))))
  (reverse acc))

(define (response-request-model r)
  (jget (or-empty-hash (jget (or-empty-hash (jget r 'metadata)) 'request_metadata)) 'model))

(define (sandbox-base d t)
  (define modes (distinct-strings (list response-request-model
                                        (lambda (r) (jget (or-empty-hash (jget r 'requestMetadata)) 'model)))
                                  d))
  (define models (distinct-strings (list (lambda (r) (jget r 'model))) d))
  (define urls (sandbox-preview-urls t))
  (hasheq 'mode (if (pair? modes) (car modes) 'null)
          'modes modes
          'models models
          'isBuildMode (and (pair? modes) (equal? (car modes) "build"))
          'previewUrls urls
          'previewUrlCount (length urls)
          'terminalSessions (sandbox-terminal-sessions t)
          'deployment 'null))

(define (sandbox-empty)
  (hasheq 'mode 'null 'modes '() 'models '() 'isBuildMode #f
          'previewUrls '() 'previewUrlCount 0 'terminalSessions 0 'deployment 'null))

;; record the sandbox facts of a parsed payload, keeping a deployment answer already obtained.
(define (record-sandbox! R d t)
  (define prev (run-sandbox R))
  (define dep (if (hash? prev) (jget prev 'deployment) 'null))
  (set-run-sandbox! R (hash-set (sandbox-base d t) 'deployment dep)))

(define (record-deployment! R dep)
  (define prev (run-sandbox R))
  (set-run-sandbox! R (hash-set (if (hash? prev) prev (sandbox-empty)) 'deployment dep)))

;; Successful deployment bodies have appeared with the record at the root and under deployment,
;; appDeployment, record, or data.  Preserve the full body, select that record without reshaping it,
;; and then find file-bearing containers by semantic name.  Each supplied reference gets an explicit
;; result; only bytes actually fetched or decoded become a local file with length and SHA-256.
(define (first-present h keys)
  (for/first ([k (in-list keys)]
              #:when (let ([v (jget h k)]) (not (eq? v 'null))))
    (jget h k)))

(define FILE-CONTAINER-KEYS '(files builtFiles built_files outputFiles output_files artifacts assets))
(define FILE-NAME-KEYS '(name fileName filename))
(define FILE-URL-KEYS '(url downloadUrl download_url fileUrl file_url href uri))
(define FILE-PATH-KEYS '(path filePath file_path))
(define FILE-MIME-KEYS '(mimeType mime_type contentType content_type))
(define FILE-SIZE-KEYS '(sizeBytes size_bytes size))
(define FILE-DATA-KEYS '(contentBase64 content_base64 dataBase64 data_base64))

(define (file-object? v)
  (and (hash? v)
       (for/or ([k (in-list (append FILE-NAME-KEYS FILE-URL-KEYS FILE-PATH-KEYS FILE-DATA-KEYS))])
         (not (eq? (jget v k) 'null)))))

(define (deployment-record-value body)
  (define v (and (hash? body) (first-present body '(deployment appDeployment app_deployment record data))))
  (if (or (hash? v) (list? v)) v body))

(define (key-name k) (if (symbol? k) (symbol->string k) (format "~a" k)))

;; -> list of (list source-path fallback-name raw-item), stable for arrays and sorted for object maps.
(define (deployment-file-candidates record)
  (define found '())
  (define (add! path fallback item) (set! found (cons (list path fallback item) found)))
  (define (walk node path)
    (cond
      [(hash? node)
       (for ([k (in-list (sort (hash-keys node) string<? #:key key-name))])
         (define v (hash-ref node k))
         (define p (format "~a.~a" path (key-name k)))
         (cond
           [(member k FILE-CONTAINER-KEYS)
            (cond
              [(list? v)
               (for ([item (in-list v)] [i (in-naturals)]) (add! (format "~a[~a]" p i) #f item))]
              [(or (file-object? v) (string? v)) (add! p #f v)]
              [(hash? v)
               (for ([fk (in-list (sort (hash-keys v) string<? #:key key-name))])
                 (add! (format "~a.~a" p (key-name fk)) (key-name fk) (hash-ref v fk)))])]
           [(or (hash? v) (list? v)) (walk v p)]))]
      [(list? node) (for ([v (in-list node)] [i (in-naturals)]) (walk v (format "~a[~a]" path i)))]))
  (walk record "$.record")
  (reverse found))

(define (reachable-url s)
  (cond
    [(and (string? s) (regexp-match? #px"^https?://" s)) s]
    [(and (string? s) (regexp-match? #px"^data:" s)) s]
    [(and (string? s) (string-prefix? s "/")) (string-append "https://grok.com" s)]
    [else #f]))

(define (path-leaf s)
  (and (string? s)
       (let* ([clean (car (regexp-split #px"[?#]" s))]
              [parts (filter (lambda (x) (not (string=? x ""))) (regexp-split #px"[/\\\\]" clean))])
         (and (pair? parts) (last parts)))))

(define (deployment-file-result R c index candidate)
  (define source-path (car candidate))
  (define fallback-name (cadr candidate))
  (define item (caddr candidate))
  (define nv (and (hash? item) (first-present item FILE-NAME-KEYS)))
  (define uv (and (hash? item) (first-present item FILE-URL-KEYS)))
  (define pv (and (hash? item) (first-present item FILE-PATH-KEYS)))
  (define mv (and (hash? item) (first-present item FILE-MIME-KEYS)))
  (define sv (and (hash? item) (first-present item FILE-SIZE-KEYS)))
  (define dv (and (hash? item) (first-present item FILE-DATA-KEYS)))
  (define raw-ref (and (string? item) item))
  (define url (reachable-url (if (string? uv) uv raw-ref)))
  (define source-file-path
    (cond [(string? pv) pv]
          [(and raw-ref (not url)) raw-ref]
          [else 'null]))
  (define name (py-or nv fallback-name (path-leaf (if (string? source-file-path) source-file-path url)) "file"))
  (define base
    (hasheq 'index index
            'sourcePath source-path
            'source item
            'name name
            'mimeType (if (string? mv) mv 'null)
            'expectedSizeBytes (if (exact-integer? sv) sv 'null)
            'sourceFilePath source-file-path
            'url (or url 'null)))
  (define (write-bytes bs acquisition)
    (define rel (format "deployment-files/~a-~a" index (safe-file-name name)))
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (hash-set* base 'status "write-failed" 'downloaded #f 'acquisition acquisition
                                  'error (exn-message e)))])
      (emit! R rel bs)
      (define result
        (hash-set* base 'status "ok" 'downloaded #t 'acquisition acquisition 'file rel
                   'sizeBytes (bytes-length bs) 'sha256 (sha256-hex bs)))
      (if (exact-integer? sv) (hash-set result 'sizeMatches (= sv (bytes-length bs))) result)))
  (cond
    [(and (string? dv) (> (string-length dv) 0))
     (with-handlers ([exn:fail?
                      (lambda (e)
                        (hash-set* base 'status "failed" 'downloaded #f 'acquisition "inline-base64"
                                   'error (format "invalid inline base64: ~a" (exn-message e))))])
       (write-bytes (base64->bytes dv) "inline-base64"))]
    [(not url)
     (hash-set* base 'status "unavailable" 'downloaded #f
                'error "deployment response supplied no reachable URL or inline bytes")]
    [(not c)
     (hash-set* base 'status "unavailable" 'downloaded #f
                'error "no browser session available to fetch deployment file")]
    [else
     (define res
       (with-handlers ([exn:fail? (lambda (e) (cons 'error (exn-message e)))])
         (cdp-run-async-function c FETCH-B64-JS url #:timeout 300)))
     (cond
       [(pair? res)
        (warn! R "deployment file ~a (~a) download failed: ~a" index name (cdr res))
        (hash-set* base 'status "failed" 'downloaded #f 'acquisition "page-fetch" 'error (cdr res))]
       [(not (= (int-or (jget res 'status) 0) 200))
        (warn! R "deployment file ~a (~a): HTTP ~a" index name (jget res 'status))
        (hash-set* base 'status "http-error" 'downloaded #f 'acquisition "page-fetch"
                   'httpStatus (jget res 'status) 'error "deployment file returned a non-200 HTTP status")]
       [else
        (hash-set (write-bytes (base64->bytes (jstr (jget res 'data))) "page-fetch")
                  'httpStatus (jget res 'status))])]))

(define (record-deployment-response! R c url status bs #:queried queried? #:injected [injected? #f]
                                     #:receipt-source [receipt-source #f])
  (api-save! R "app-deployments" "api-app-deployments.json" url status bs)
  (define body (with-handlers ([exn:fail? (lambda (e) 'null)]) (bytes->jsexpr bs)))
  (define record (deployment-record-value body))
  (define found (and (= status 200) (or (hash? record) (list? record))))
  (define built
    (if found
        (for/list ([candidate (in-list (deployment-file-candidates record))] [i (in-naturals 1)])
          (deployment-file-result R c i candidate))
        '()))
  (record-deployment!
   R
   (hash-set*
    (hasheq 'endpoint url
            'queried queried?
            'status status
            'found found
            'record (if found record 'null)
            'body body
            'message (if (hash? body) (jget body 'message) 'null)
            'builtFiles built
            'note (if found
                      "deployment record preserved; every supplied file reference has an explicit acquisition outcome"
                      "no successful deployment record was returned; no built files were acquired")
            'file "raw/api-app-deployments.json")
    'injected injected?
    'receiptSource (or receipt-source 'null))))

;; SPEC 0.6: GET /rest/app-chat/app-deployments?latest_by_conversation_id=<id>.
(define (query-app-deployment! R c conv-id)
  (define url (format "https://grok.com/rest/app-chat/app-deployments?latest_by_conversation_id=~a" conv-id))
  (define t0 (now-ms))
  (define res (with-handlers ([exn:fail? (lambda (e) (cons 'error (exn-message e)))])
                (page-fetch c url)))
  (cond
    [(pair? res)
     (warn! R "app-deployments query failed: ~a" (cdr res))
     (record-deployment! R (hasheq 'endpoint url 'queried #f 'reason (format "request failed: ~a" (cdr res))))]
    [else
     (define status (int-or (jget res 'status) 0))
     (define bs (string->bytes/utf-8 (jstr (jget res 'text))))
     (record-deployment-response! R c url status bs #:queried #t)
     (define found (= status 200))
     (log-info "app-deployments: HTTP ~a, ~a bytes~a (~a ms)" status (bytes-length bs)
               (if found ", deployment record kept in manifest.json" ", no deployment record")
               (- (now-ms) t0))
     (timing! R "app-deployments" t0)]))

;; ------------------------------------------------------------------ manifest
(define (verification-summary R)
  (define v (run-verification R))
  (cond
    [(hash? v)
     (define s (or-empty-hash (jget v 'summary)))
     (hasheq 'file "verification.json"
             'ok (jget v 'ok)
             'total (jget s 'total) 'passed (jget s 'ok) 'failed (jget s 'failed)
             'failedChecks (or-empty-list (jget v 'failedChecks))
             'api (jget v 'api)
             'dom (jget v 'dom)
             'warnings (length (or-empty-list (jget v 'warnings))))]
    [else "not run"]))

(define (behavior-manifest-record R)
  (define report (run-behavior R))
  (define summary
    (and (hash? report)
         (hash? (jget report 'summary))
         (hash-set (jget report 'summary)
                   'findingCount
                   (length (or-empty-list (jget report 'findings))))))
  (hasheq 'status (run-behavior-status R)
          'schemaVersion behavior:schema-version
          'detectorVersion behavior:detector-version
          'files (hasheq 'json "behavior-report.json"
                         'markdown "behavior-report.md")
          'summary (or summary 'null)
          'durationMs (run-behavior-ms R)
          'error (or (run-behavior-error R) 'null)))

;; Analyze the final normalized transcript.  The detector is additive: it emits
;; its own two files and never edits any transcript artifact.
(define (write-behavior-reports! R transcript)
  (define started (now-ms))
  (set-run-behavior-status! R "error")
  (set-run-behavior! R 'null)
  (set-run-behavior-error! R #f)
  (define ok?
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (set-run-behavior-error! R (exn-message e))
                       (warn! R "behavior report failed: ~a" (exn-message e))
                       #f)])
      (unless (hash? transcript)
        (error 'behavior-report "final transcript is missing or malformed"))
      (define report (behavior:analyze-transcript transcript))
      (emit-string! R "behavior-report.json" (behavior:report-json-string report))
      (emit-string! R "behavior-report.md" (behavior:render-markdown report))
      (set-run-behavior! R report)
      (set-run-behavior-status! R "ok")
      (define summary (or-empty-hash (jget report 'summary)))
      (log-info "behavior report: ~a finding(s), ~a confirmed, ~a candidate, ~a not assessable"
                (length (or-empty-list (jget report 'findings)))
                (jget summary 'confirmed)
                (jget summary 'candidate)
                (jget summary 'notAssessable))
      (log-info "behavior report: ~a"
                (path->string (build-path (run-export-dir R) "behavior-report.json")))
      (log-info "behavior report: ~a"
                (path->string (build-path (run-export-dir R) "behavior-report.md")))
      #t))
  (define elapsed (- (now-ms) started))
  (set-run-behavior-ms! R elapsed)
  (set-run-timings!
   R
   (append (filter (lambda (entry) (not (equal? (car entry) "behaviorReportMs")))
                   (run-timings R))
           (list (cons "behaviorReportMs" elapsed))))
  ok?)

(define (write-manifest! R exit-code #:error [err #f])
  (define opts (run-opts R))
  (define m
    (hasheq 'tool TOOL-NAME
            'version TOOL-VERSION
            'language "racket"
            'racketVersion (version)
            'extractJsSha256 extract-js-sha256
            'extractJsSize extract-js-size
            'args (vector->list (current-command-line-arguments))
            'options (hasheq 'out (path->string* (hash-ref opts 'out))
                             'port (hash-ref opts 'port)
                             'chrome (hash-ref opts 'chrome)
                             'profile (hash-ref opts 'profile)
                             'noLaunch (hash-ref opts 'no-launch)
                             'keepOpen (hash-ref opts 'keep-open)
                             'timeoutSec (hash-ref opts 'timeout)
                             'skipDom (hash-ref opts 'skip-dom)
                             'skipApi (hash-ref opts 'skip-api)
                             'rounds (hash-ref opts 'rounds)
                             'fromJson (or (hash-ref opts 'from-json) 'null)
                             'sourceUrl (or (hash-ref opts 'source-url) 'null)
                             'url (or (hash-ref opts 'url) 'null))
            'startedAt (run-started-at R)
            'finishedAt (iso-now-utc)
            'totalMs (- (now-ms) (run-t0 R))
            'timingsMs (for/hasheq ([t (in-list (run-timings R))]) (values (string->symbol (car t)) (cdr t)))
            'conversationId (run-conversation-id R)
            'finalUrl (run-final-url R)
            'apiLane (run-api-lane R)
            'parsedFormat (run-parsed-format R)
            'transport (run-transport R)
            'apiRequests (for/list ([r (in-list (run-raws R))])
                           (hasheq 'name (hash-ref r 'name) 'url (hash-ref r 'url)
                                   'status (hash-ref r 'status) 'bytes (hash-ref r 'bytes)
                                   'file (hash-ref r 'file)))
            'counts (run-counts R)
            'domLane (hasheq 'rounds (for/list ([d (in-list (run-dom-rounds R))])
                                       (for/hasheq ([(k v) (in-hash d)] #:unless (eq? k 'capture)) (values k v)))
                             'screenshots (run-screenshots R)
                             'citationChipsHelper (if (hash? (run-chip-links R)) "raw/citation-chips-round1.json" 'null))
            'sandbox (run-sandbox R)
            'citationEnrichment (run-enrichment R)
            'citationSources (run-citation-sources R)
            'verification (verification-summary R)
            'behavior (behavior-manifest-record R)
            'attachments (for/list ([(fid rel) (in-hash (run-attachment-paths R))]) (hasheq 'fileId fid 'path rel))
            'warnings (run-warnings R)
            'notes (run-notes R)
            'error (or err 'null)
            'exitCode exit-code
            'files (run-files R)))
  (define full (build-path (run-export-dir R) "manifest.json"))
  (write-file-string full (jsexpr->canonical-string m))
  full)

;; ------------------------------------------------------------------ citation sources (SPEC 0.5)
;; Which source filled each citation's url/kind:
;;   "api-card"  legacy format, resolved from the response's cardAttachmentsJson (card id match)
;;   "api-chunk" chunk format, carried by the renderCitation chunk itself
;;   "dom-chip"  still null after the API step, filled by the DOM chip fallback (enrich-citations)
;;   "none"      unresolved (null url/kind in transcript.json)
;; t-api: the transcript as built from the API payload; t-final: after the DOM fallback (may be eq).
(define (citation-sources R t-api t-final)
  (define api-label (if (equal? (run-parsed-format R) "chunk") "api-chunk" "api-card"))
  (define counts (make-hash (list (cons "api-card" 0) (cons "api-chunk" 0) (cons "dom-chip" 0) (cons "none" 0))))
  (define final-turns (list->vector (or-empty-list (jget t-final 'turns))))
  (define turns
    (for/list ([turn (in-list (or-empty-list (jget t-api 'turns)))] [k (in-naturals)]
               #:when (pair? (or-empty-list (jget turn 'citations))))
      (define cits-api (or-empty-list (jget turn 'citations)))
      (define cits-final
        (if (< k (vector-length final-turns)) (or-empty-list (jget (vector-ref final-turns k) 'citations)) '()))
      (hasheq 'turnIndex (jget turn 'index)
              'citations
              (for/list ([c cits-api] [j (in-naturals)])
                (define c* (if (< j (length cits-final)) (list-ref cits-final j) c))
                (define src
                  (cond [(truthy? (jget c 'url)) api-label]
                        [(truthy? (jget c* 'url)) "dom-chip"]
                        [else "none"]))
                (hash-update! counts src add1 0)
                (hasheq 'cardId (jget c 'cardId) 'citationId (jget c 'citationId)
                        'url (jget c* 'url) 'kind (jget c* 'kind) 'source src)))))
  (hasheq 'rule "url/kind come from the API (cardAttachmentsJson card id on the legacy format, renderCitation chunk on the chunk format); DOM chips fill only citations the API left null"
          'summary (for/hasheq ([(k v) (in-hash counts)]) (values (string->symbol k) v))
          'turns turns))

;; ------------------------------------------------------------------ transcript writing
(define (count-transcript R t)
  (define turns (or-empty-list (jget t 'turns)))
  (set-run-counts! R
                   (hasheq 'turns (length turns)
                           'assistantTurns (for/sum ([tu turns]) (if (equal? (jget tu 'sender) "assistant") 1 0))
                           'humanTurns (for/sum ([tu turns]) (if (equal? (jget tu 'sender) "human") 1 0))
                           'citations (for/sum ([tu turns]) (length (or-empty-list (jget tu 'citations))))
                           'citationsWithUrl (for*/sum ([tu turns] [c (or-empty-list (jget tu 'citations))]) (if (truthy? (jget c 'url)) 1 0))
                           'attachments (for/sum ([tu turns]) (length (or-empty-list (jget tu 'attachments))))
                           'summaries (for*/sum ([tu turns]
                                                 [rl (or-empty-list (jget (or-empty-hash (jget tu 'thinking)) 'rollouts))]
                                                 [ev (or-empty-list (jget rl 'events))])
                                        (if (equal? (jget ev 'type) "summary") 1 0))
                           'toolEvents (for*/sum ([tu turns]
                                                  [rl (or-empty-list (jget (or-empty-hash (jget tu 'thinking)) 'rollouts))]
                                                  [ev (or-empty-list (jget rl 'events))])
                                         (if (equal? (jget ev 'type) "tool") 1 0)))))

;; Write transcript.json + transcript.md (+ transcript.html when asked) from a transcript jsexpr.
(define (write-transcript-outputs! R t #:html [html? #t])
  (define t-write (now-ms))
  (define json-str (jsexpr->canonical-string t))
  (define md-str (transcript->markdown t))
  (emit-string! R "transcript.json" json-str)
  (emit-string! R "transcript.md" md-str)
  (when html?
    (emit-string! R "transcript.html"
                  (transcript->html t #:attachment-paths (run-attachment-paths R)
                                    #:generator (format "~a ~a" TOOL-NAME TOOL-VERSION))))
  (timing! R "write-transcript" t-write)
  (count-transcript R t)
  (log-info "transcript.json: ~a bytes, transcript.md: ~a bytes~a"
            (bytes-length (string->bytes/utf-8 json-str)) (bytes-length (string->bytes/utf-8 md-str))
            (if html? ", transcript.html written" "")))

;; Parse the API JSON into the transcript jsexpr and write the transcript files.
(define (parse-and-write-transcript! R d source-url #:html [html? #t])
  (define t-parse (now-ms))
  (define fmt (transcript-format d))
  (define t (build-transcript d source-url))
  (timing! R "parse" t-parse)
  (set-run-parsed-format! R fmt)
  (define conv (jget t 'conversation))
  (set-run-conversation-id! R (jget conv 'conversationId))
  (record-sandbox! R d t)
  (write-transcript-outputs! R t #:html html?)
  (log-info "parsed format ~a, ~a turns" fmt (length (or-empty-list (jget t 'turns))))
  t)

;; ------------------------------------------------------------------ developer modes
(define (run-from-json R)
  (define opts (run-opts R))
  (define src (hash-ref opts 'from-json))
  (define out-dir (simplify-path (path->complete-path (hash-ref opts 'out))))
  (define t-read (now-ms))
  (define bs (read-file-bytes src))
  (define d (bytes->jsexpr bs))
  (timing! R "read-json" t-read)
  (ensure-dir! out-dir)
  (set-run-export-dir! R out-dir)
  (set-run-api-lane! R "from-json")
  (define t (parse-and-write-transcript! R d (hash-ref opts 'source-url)))
  (define deployment-json (hash-ref opts 'deployment-json))
  (if deployment-json
      (let* ([conv-id (run-conversation-id R)]
             [url (format "https://grok.com/rest/app-chat/app-deployments?latest_by_conversation_id=~a" conv-id)]
             [dep-bs (read-file-bytes deployment-json)])
        (record-deployment-response! R #f url 200 dep-bs #:queried #f #:injected #t
                                     #:receipt-source deployment-json))
      (record-deployment! R (hasheq 'endpoint 'null 'queried #f
                                    'reason "--from-json: no browser session, app-deployments was not queried")))
  (set-run-citation-sources! R (citation-sources R t t))
  (define behavior-ok? (write-behavior-reports! R t))
  (define exit-code (if behavior-ok? 0 1))
  (write-manifest! R exit-code
                   #:error (and (not behavior-ok?)
                                (format "behavior report failed: ~a"
                                        (or (run-behavior-error R) "unknown error"))))
  (log-info "Wrote ~a, ~a and ~a"
            (path->string (build-path out-dir "transcript.json"))
            (path->string (build-path out-dir "transcript.md"))
            (path->string (build-path out-dir "transcript.html")))
  exit-code)

(define (file-basename p)
  (define-values (base name dir?) (split-path (path->complete-path p)))
  (path->string name))

;; --verify-transcript T --verify-capture C [--verify-capture C2 ...] [--attachments DIR] --out FILE
(define (run-verify-only R)
  (define opts (run-opts R))
  (define tp (hash-ref opts 'verify-transcript))
  (define caps (hash-ref opts 'verify-captures))
  (when (null? caps) (error 'args "--verify-transcript needs at least one --verify-capture"))
  (define out (hash-ref opts 'out))
  (when (equal? out DEFAULT-OUT) (error 'args "--verify-transcript needs --out <verification.json>"))
  (define t (bytes->jsexpr (read-file-bytes tp)))
  (define captures (for/list ([c caps]) (bytes->jsexpr (read-file-bytes c))))
  (define stability
    (for/list ([c (cdr captures)] [name (cdr caps)])
      (stability-check (car captures) c (file-basename (car caps)) (file-basename name))))
  (define res (verify-capture t (car captures)
                              #:attachments-dir (hash-ref opts 'attachments)
                              #:transcript-name (file-basename tp)
                              #:capture-name (file-basename (car caps))
                              #:tool (format "~a ~a verify" TOOL-NAME TOOL-VERSION)
                              #:extra-checks stability))
  (write-file-string out (jsexpr->canonical-string res))
  (define s (hash-ref res 'summary))
  (log-info "checks: ~a, ok: ~a, failed: ~a -> ~a" (hash-ref s 'total) (hash-ref s 'ok) (hash-ref s 'failed) out)
  (for ([c (hash-ref res 'failedChecks)]) (log-info "  FAILED ~a turn ~a" (hash-ref c 'name) (hash-ref c 'turnIndex)))
  (for ([w (hash-ref res 'warnings)]) (log-info "  WARNING ~a" w))
  (if (hash-ref res 'ok) 0 2))

;; ------------------------------------------------------------------ page-context helpers
(define POLL-JS
  (string-append
   "(() => { const arts = document.querySelectorAll('[role=article]').length;"
   " const body = document.body ? document.body.innerText : '';"
   " const p = location.pathname;"
   " return { href: location.href, n: arts, main: !!document.querySelector('main'),"
   " signin: /\\/(sign-in|login)/i.test(p) || (arts === 0 && /\\bSign in\\b/.test(body)) }; })()"))

;; fetch(url, init) in page context -> (hasheq 'status n 'text s)
(define (page-fetch c url #:method [method "GET"] #:body [body #f] #:timeout [timeout 180])
  (define init
    (if body
        (hasheq 'credentials "include" 'method method
                'headers (hasheq 'content-type "application/json") 'body body)
        (hasheq 'credentials "include" 'method method)))
  (define js
    (format "(async () => { const r = await fetch(~a, ~a); const t = await r.text(); return { status: r.status, text: t }; })()"
            (jsexpr->string url) (jsexpr->string init)))
  (define v (cdp-eval c js #:timeout timeout))
  (unless (hash? v) (error 'page-fetch "unexpected fetch result for ~a: ~s" url v))
  v)

;; Download a URL from page context as base64 -> (hasheq 'status 'type 'size 'data)
(define FETCH-B64-JS
  (string-append
   "(async (url) => { const r = await fetch(url, { credentials: 'include' });"
   " if (r.status !== 200) return { status: r.status };"
   " const b = await r.blob();"
   " const d = await new Promise((res, rej) => { const fr = new FileReader();"
   " fr.onload = () => res(fr.result); fr.onerror = () => rej(fr.error); fr.readAsDataURL(b); });"
   " return { status: r.status, type: b.type, size: b.size, data: d.slice(d.indexOf(',') + 1) }; })"))

;; Group-chip helper (legacy citation enrichment): every `button.inline` chip in an assistant reply
;; is a group of citations; clicking it replaces the button by the individual a.citation links.
;; Returns {"<articleIndex>": [{chip, text, size, links:[{href,text}]}], elapsedMs}.
(define CHIP-LINKS-JS
  (string-append
   "(async () => {"
   " const yieldTask = () => new Promise((r) => { const ch = new MessageChannel(); ch.port1.onmessage = () => { ch.port1.close(); r(); }; ch.port2.postMessage(0); });"
   " const sleep = async (ms) => { if (document.visibilityState === 'visible') return new Promise((r) => setTimeout(r, ms));"
   "   const end = performance.now() + ms; while (performance.now() < end) await yieldTask(); };"
   " const norm = (s) => (s || '').replace(/[\\u200B\\u2060\\uFEFF]/g, '').replace(/\\s+/g, ' ').trim();"
   " const t0 = Date.now(); const out = {};"
   " const arts = Array.from(document.querySelectorAll('[role=article]'));"
   " for (let i = 0; i < arts.length; i++) {"
   "   const mds = arts[i].querySelectorAll('.response-content-markdown'); const md = mds[mds.length - 1]; if (!md) continue;"
   "   const chips = Array.from(md.querySelectorAll('a.citation, button.inline'));"
   "   const recs = [];"
   "   for (let k = 0; k < chips.length; k++) {"
   "     const c = chips[k]; if (c.tagName !== 'BUTTON') continue;"
   "     const text = norm(c.innerText); const m = /\\+(\\d+)\\s*$/.exec(text); const size = 1 + (m ? parseInt(m[1], 10) : 0);"
   "     const before = new Set(Array.from(md.querySelectorAll('a[href]')));"
   "     c.click();"
   "     let links = [];"
   "     for (let w = 0; w < 30; w++) { await sleep(100); links = Array.from(md.querySelectorAll('a[href]')).filter((a) => !before.has(a)); if (links.length >= size) break; }"
   "     recs.push({ chip: k, text, size, links: links.map((a) => ({ href: a.getAttribute('href'), text: norm(a.innerText) })) });"
   "   }"
   "   if (recs.length) out[String(i)] = recs;"
   " }"
   " try { document.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', code: 'Escape', bubbles: true })); } catch (e) {}"
   " out.elapsedMs = Date.now() - t0;"
   " return out; })()"))

(define CLOSE-ASIDE-JS
  "(() => { const b = document.querySelector('aside button[aria-label=\"Close\"]'); if (b) { b.click(); return true; } return false; })()")

;; Step 2: wait until the conversation is rendered (articles present and stable).
(define (wait-for-conversation R c)
  (define opts (run-opts R))
  (define timeout-s (hash-ref opts 'timeout))
  (define start (now-ms))
  (define deadline (+ start (* 1000 timeout-s)))
  (define login-printed? #f)
  (let loop ([last-n -1] [streak 0] [polls 0])
    (define st
      (with-handlers ([exn:fail? (lambda (e)
                                   (log-verbose "poll failed: ~a" (exn-message e))
                                   (hasheq 'href "" 'n 0 'main #f 'signin #f))])
        (cdp-eval c POLL-JS #:timeout 30)))
    (define n (int-or (jget st 'n) 0))
    (define href (jstr (jget st 'href)))
    (define streak* (if (and (> n 0) (= n last-n)) (add1 streak) (if (> n 0) 1 0)))
    (log-verbose "poll ~a: articles=~a main=~a href=~a" polls n (jget st 'main) href)
    (cond
      [(>= streak* 3)
       (log-info "Conversation rendered: ~a articles, url ~a (~a ms)" n href (- (now-ms) start))
       (values href n)]
      [(> (now-ms) deadline)
       (error 'wait "conversation did not appear within ~a s (last url ~a, articles ~a)" timeout-s href n)]
      [else
       (when (and (not login-printed?) (= n 0) (> (- (now-ms) start) 15000) (truthy? (jget st 'signin)))
         (set! login-printed? #t)
         (log-info "Waiting for you to log in to grok.com in the Chrome window (profile ~a) … ~as left"
                   (hash-ref opts 'profile) (quotient (- deadline (now-ms)) 1000))
         (report! 'login (list (quotient (- deadline (now-ms)) 1000) (hash-ref opts 'profile))))
       (sleep 0.5)
       (loop n streak* (add1 polls))])))

(define (set-viewport! R c)
  (with-handlers ([exn:fail? (lambda (e) (warn! R "Emulation.setDeviceMetricsOverride failed: ~a" (exn-message e)))])
    (cdp-call c "Emulation.setDeviceMetricsOverride"
              (hasheq 'width 1600 'height 1200 'deviceScaleFactor 1 'mobile #f))))

;; ------------------------------------------------------------------ API lane
(define HYDRATE-ATTEMPTS 3)
(define HYDRATE-DELAY-S 2)
(define API-LATE-RETRIES 3)
(define API-LATE-RETRY-DELAY-S 30)

;; Fetch one endpoint from page context -> (values parsed-jsexpr-or-#f http-status body-bytes-or-#f).
(define (api-fetch-raw R c name url #:method [method "GET"] #:body [body #f])
  (define t (now-ms))
  (define res
    (with-handlers ([exn:fail? (lambda (e)
                                 (warn! R "API ~a (~a ~a) failed: ~a" name method url (exn-message e))
                                 #f)])
      (page-fetch c url #:method method #:body body)))
  (cond
    [(not res) (values #f 0 #f)]
    [else
     (define status (int-or (jget res 'status) 0))
     (define bs (string->bytes/utf-8 (jstr (jget res 'text))))
     (log-info "API ~a: HTTP ~a, ~a bytes (~a ms)" name status (bytes-length bs) (- (now-ms) t))
     (unless (= status 200)
       (warn! R "API ~a returned HTTP ~a (~a)" name status url))
     (define j (and (= status 200)
                    (with-handlers ([exn:fail? (lambda (e)
                                                 (warn! R "API ~a: body is not JSON: ~a" name (exn-message e))
                                                 #f)])
                      (bytes->jsexpr bs))))
     (values j status bs)]))

;; Save a fetched payload verbatim as raw/<file> and record it for the manifest.
(define (api-save! R name file url status bs)
  (define rel (string-append "raw/" file))
  (queue-raw! R rel bs)
  (set-run-raws! R (append (run-raws R)
                           (list (hasheq 'name name 'url url 'status status
                                         'bytes (bytes-length bs) 'file rel))))
  (log-info "  -> ~a (~a bytes)" rel (bytes-length bs)))

;; Fetch one endpoint, save the verbatim body as raw/api-<name>.json, return
;; (values parsed-jsexpr-or-#f http-status).  With #:hydrated #t a payload whose responses
;; list xpostIds but carry no xposts (a server-side hydration failure seen on grok.com) is
;; re-fetched up to HYDRATE-ATTEMPTS times; every rejected attempt is kept as
;; raw/api-<name>.attempt<k>-unhydrated.json and the final payload is used regardless.
(define (api-fetch R c name url #:method [method "GET"] #:body [body #f] #:hydrated [hydrated? #f])
  (define (once) (api-fetch-raw R c name url #:method method #:body body))
  (define-values (j status bs attempts reason)
    (if hydrated?
        (fetch-until-hydrated
         once
         #:max-attempts HYDRATE-ATTEMPTS
         #:delay-s HYDRATE-DELAY-S
         #:on-reject (lambda (attempt reason j0 status0 bs0)
                       (when bs0
                         (api-save! R name (format "api-~a.attempt~a-unhydrated.json" name attempt) url status0 bs0))
                       (warn! R "API ~a attempt ~a/~a: ~a; re-fetching in ~a s"
                              name attempt HYDRATE-ATTEMPTS reason HYDRATE-DELAY-S)))
        (let-values ([(j status bs) (once)]) (values j status bs 1 #f))))
  (when reason
    (warn! R "API ~a: ~a after ~a attempts; the export keeps the payload as served (X posts will be missing)"
           name reason attempts))
  (when bs (api-save! R name (format "api-~a.json" name) url status bs))
  (values j status))

(define (has-responses? j)
  (and (hash? j) (pair? (or-empty-list (jget j 'responses)))))

;; /share/<linkId>: both chunk and legacy.  -> (values chosen chunk-or-#f legacy-or-#f)
(define (api-lane-share R c link-id #:suffix [suffix ""])
  (set-run-api-lane! R "share")
  (define base (format "https://grok.com/rest/app-chat/share_links/~a" link-id))
  (define-values (chunk s1) (api-fetch R c (string-append "share_links_chunk" suffix) (string-append base "?useChunk=true") #:hydrated #t))
  (define-values (legacy s2) (api-fetch R c (string-append "share_links_legacy" suffix) base #:hydrated #t))
  (define chosen
    (cond
      [(and (has-responses? chunk) (equal? (transcript-format chunk) "chunk")) chunk]
      [(has-responses? legacy) legacy]
      [(has-responses? chunk) chunk]
      [else #f]))
  (values chosen (and (has-responses? chunk) chunk) (and (has-responses? legacy) legacy)))

;; /c/<conversationId> (logged in): responses (+useChunk), response-node, metadata,
;; load-responses fallback.
(define (api-lane-conversation R c conv-id #:suffix [suffix ""])
  (set-run-api-lane! R "conversation")
  (define base (format "https://grok.com/rest/app-chat/conversations/~a" conv-id))
  (define-values (meta s0) (api-fetch R c (string-append "conversation" suffix) base))
  (define-values (legacy s1) (api-fetch R c (string-append "responses" suffix) (string-append base "/responses?includeThreads=false") #:hydrated #t))
  (define-values (chunk s2) (api-fetch R c (string-append "responses_chunk" suffix) (string-append base "/responses?includeThreads=false&useChunk=true") #:hydrated #t))
  (define-values (nodes s3) (api-fetch R c (string-append "response-node" suffix) (string-append base "/response-node")))
  (define loaded
    (cond
      [(or (has-responses? legacy) (has-responses? chunk)) #f]
      [(and (hash? nodes) (pair? (or-empty-list (jget nodes 'responseNodes))))
       (define ids (for/list ([n (or-empty-list (jget nodes 'responseNodes))]
                              #:when (string? (jget n 'responseId)))
                     (jget n 'responseId)))
       (define-values (lr s4)
         (api-fetch R c (string-append "load-responses" suffix) (string-append base "/load-responses")
                    #:method "POST" #:body (jsexpr->string (hasheq 'responseIds ids)) #:hydrated #t))
       lr]
      [else #f]))
  (define responses-src
    (cond
      [(and (has-responses? chunk) (equal? (transcript-format chunk) "chunk")) chunk]
      [(has-responses? legacy) legacy]
      [(has-responses? chunk) chunk]
      [(has-responses? loaded) loaded]
      [else #f]))
  (cond
    [(not responses-src) #f]
    [else
     (define conv-meta
       (cond [(and (hash? meta) (hash? (jget meta 'conversation))) (jget meta 'conversation)]
             [(hash? meta) meta]
             [(hash? (jget responses-src 'conversation)) (jget responses-src 'conversation)]
             [else (hasheq 'conversationId conv-id)]))
     (define d (make-hasheq))
     (for ([(k v) (in-hash responses-src)]) (hash-set! d k v))
     (hash-set! d 'conversation conv-meta)
     d]))

;; ------------------------------------------------------------------ attachments (step 6)
(define (download-attachments! R c t)
  (define t0 (now-ms))
  (define n-ok 0)
  (define n-total 0)
  (define paths (make-hash))
  (define n-planned
    (for/sum ([turn (in-list (or-empty-list (jget t 'turns)))])
      (length (or-empty-list (jget turn 'attachments)))))
  (for ([turn (in-list (or-empty-list (jget t 'turns)))])
    (for ([a (in-list (or-empty-list (jget turn 'attachments)))])
      (set! n-total (add1 n-total))
      (phase! (format "Downloading attachment ~a of ~a…" n-total n-planned)
              (+ 0.30 (* 0.03 (/ (exact->inexact n-total) (max 1 n-planned)))))
      (define url (jget a 'contentUrl))
      (define rel (format "attachments/~a-~a-~a" (jget turn 'index) (safe-file-name (py-str (jget a 'fileId)))
                          (safe-file-name (py-str (jget a 'fileName)))))
      (cond
        [(not (string? url)) (warn! R "attachment ~a has no contentUrl" rel)]
        [else
         ;; up to 3 attempts: a network error ("Failed to fetch"), HTTP 429 or 5xx is retried after 1 s, then 3 s
         (define res
           (let retry ([attempt 1])
             (define r (with-handlers ([exn:fail? (lambda (e) (cons 'error (exn-message e)))])
                         (cdp-run-async-function c FETCH-B64-JS url #:timeout 300)))
             (define st (and (hash? r) (int-or (jget r 'status) 0)))
             (define transient (or (pair? r) (and st (or (= st 0) (= st 429) (>= st 500)))))
             (cond
               [(and transient (< attempt 3))
                (log-info "attachment ~a: attempt ~a ~a; retrying" rel attempt (if (pair? r) (cdr r) (format "HTTP ~a" st)))
                (sleep (if (= attempt 1) 1 3))
                (retry (add1 attempt))]
               [(pair? r) (warn! R "attachment ~a download failed: ~a" url (cdr r)) #f]
               [else r])))
         (cond
           [(not res) (void)]
           [(not (= (int-or (jget res 'status) 0) 200))
            (warn! R "attachment ~a: HTTP ~a" url (jget res 'status))]
           [else
            (define bs (base64->bytes (jstr (jget res 'data))))
            (emit! R rel bs)
            (hash-set! paths (jget a 'fileId) rel)
            (set! n-ok (add1 n-ok))
            (define expected (jget a 'sizeBytes))
            (if (and (exact-integer? expected) (not (= expected (bytes-length bs))))
                (warn! R "attachment ~a: downloaded ~a bytes, API sizeBytes ~a" rel (bytes-length bs) expected)
                (log-info "attachment ~a: ~a bytes (~a)~a" rel (bytes-length bs) (jget res 'type)
                          (if (exact-integer? expected) " == sizeBytes" "")))])])))
  (set-run-attachment-paths! R (for/hash ([(k v) (in-hash paths)]) (values k v)))
  (timing! R "attachments" t0)
  (values n-ok n-total))

;; ------------------------------------------------------------------ page capture
(define PNG-MAGIC #"\211PNG\r\n\32\n")

(define (screenshot-png c #:beyond-viewport [beyond? #f])
  (define shot (cdp-call c "Page.captureScreenshot"
                         (if beyond?
                             (hasheq 'format "png" 'captureBeyondViewport #t)
                             (hasheq 'format "png"))
                         #:timeout 180))
  (define png (base64->bytes (jstr (jget shot 'data))))
  (unless (and (>= (bytes-length png) 8) (equal? (subbytes png 0 8) PNG-MAGIC))
    (error 'screenshot "decoded data is not a PNG (~a bytes)" (bytes-length png)))
  png)

(define (capture-page! R c)
  (define t0 (now-ms))
  (with-handlers ([exn:fail? (lambda (e) (warn! R "page HTML capture failed: ~a" (exn-message e)))])
    (define html (cdp-eval c "document.documentElement.outerHTML" #:await #f #:timeout 180))
    (define bs (string->bytes/utf-8 (jstr html)))
    (emit! R "raw/page-initial.html" bs)
    (log-info "raw/page-initial.html: ~a bytes (single CDP reply)" (bytes-length bs)))
  (with-handlers ([exn:fail? (lambda (e) (warn! R "screenshot failed: ~a" (exn-message e)))])
    (define png (screenshot-png c #:beyond-viewport #t))
    (emit! R "screenshots/transcript-full.png" png)
    (set-run-screenshots! R (append (run-screenshots R) (list "screenshots/transcript-full.png")))
    (log-info "screenshots/transcript-full.png: ~a bytes (captureBeyondViewport, valid PNG signature)" (bytes-length png)))
  (timing! R "capture-page" t0))

;; ------------------------------------------------------------------ DOM lane (SPEC 2.5 and 4)
(define EXTRACT-TIMEOUT-S 900)

(define (run-extractor c opts)
  (cdp-run-async-function c extract-js opts #:timeout EXTRACT-TIMEOUT-S))

(define (capture-summary cap)
  (define st (or-empty-hash (jget cap 'stats)))
  (define env (or-empty-hash (jget cap 'env)))
  (hasheq 'articles (jget st 'articles) 'userArticles (jget st 'userArticles) 'assistantArticles (jget st 'assistantArticles)
          'thoughtsPanels (jget st 'thoughtsPanels) 'sourcesPanels (jget st 'sourcesPanels)
          'thoughtRows (jget st 'thoughtRows) 'sourceRows (jget st 'sourceRows)
          'thoughtLinks (jget st 'thoughtLinks) 'sourceLinks (jget st 'sourceLinks)
          'remainingCollapsedTotal (jget st 'remainingCollapsedTotal)
          'visibilityState (jget env 'visibilityState) 'rafAlive (jget env 'rafAlive)
          'forcedOpen (jget env 'forcedOpen) 'elapsedMs (jget env 'elapsedMs)
          'extractorWarnings (or-empty-list (jget env 'warnings))))

;; ------------------------------------------------------------------ DOM rounds: background extractor + shot handshake
;;
;; The extractor used to run twice per panel: once inside the round (which opens and fully expands every Thoughts and
;; Sources panel) and once more per panel afterwards, only so the panel would be open for Page.captureScreenshot.  That
;; second pass was the entire cost of the screenshots — about 170 s of a 291 s run.
;;
;; Now the round itself is photographed.  extract.js takes opts.pauseForShot: with a panel open and fully expanded it
;; publishes window.__grokShotReady = {article, panel, seq} and waits for window.__grokShotDone === seq before walking
;; it.  The extractor is therefore not run with an awaited Runtime.evaluate (nothing else could be sent on that call
;; while it ran) but started as a background promise on the page (window.__grokRun); this host polls it, photographs,
;; releases, and finally collects window.__grokRun.result.  The C app speaks the identical protocol.
;;
;; Rounds are independent captures of the same page, so rounds 1 and 2 run at the same time in two tabs, polled in turn.

(define SHOT-POLL-MS 60)
(define DOM-HYDRATION-ATTEMPTS 3)
(define DOM-HYDRATION-DELAY-S 30)

(define SHOT-START-HEAD
  (string-append "(() => { window.__grokRun = { done: false, ok: false, result: null, error: null };"
                 " window.__grokShotReady = null; window.__grokShotDone = 0; try { ("))
(define SHOT-START-TAIL
  (string-append ").then((r) => { window.__grokRun.result = r; window.__grokRun.ok = true; window.__grokRun.done = true; },"
                 " (e) => { window.__grokRun.error = String((e && e.stack) || e); window.__grokRun.done = true; }); }"
                 " catch (e) { window.__grokRun.error = String((e && e.stack) || e); window.__grokRun.done = true; }"
                 " return true; })()"))
(define SHOT-POLL-JS
  (string-append "(() => { const r = window.__grokRun || {}; const s = window.__grokShotReady;"
                 " return { done: !!r.done, error: r.error || null, article: s ? s.article : -1,"
                 " panel: s ? String(s.panel) : '', seq: s ? s.seq : 0 }; })()"))
(define SHOT-RESULT-JS
  "(() => { const r = window.__grokRun; window.__grokRun = null; return (r && r.result) || null; })()")

(struct rstate (c k [shots? #:mutable]
                [finished #:mutable] [ok #:mutable] [capture #:mutable] [error #:mutable]
                [shots #:mutable] [last-seq #:mutable] [t0 #:mutable]))

(define (make-round c k shots?) (rstate c k shots? #f #f #f #f 0 0 (now-ms)))

;; Starts (<extract.js>)(opts) as a background promise on the page; returns as soon as it is running.
(define (extractor-start! st)
  (define k (rstate-k st))
  (define opts (if (rstate-shots? st)
                   (hasheq 'round k 'settleMs 700 'maxPasses 25 'pauseForShot #t 'shotTimeoutMs 180000)
                   (hasheq 'round k 'settleMs 700 'maxPasses 25)))
  (define expr (string-append SHOT-START-HEAD extract-js ")(" (jsexpr->string opts) SHOT-START-TAIL))
  (set-rstate-t0! st (now-ms))
  (unless (eq? #t (cdp-eval (rstate-c st) expr #:await #f #:timeout 120))
    (error 'dom "DOM round ~a: the extractor did not start on the page" k))
  (log-info "DOM round ~a: extractor started in the background" k))

(define (take-panel-shot! R st art pan)
  (define rel (format "screenshots/turn~a-~a.png" art pan))
  (with-handlers ([exn:fail? (lambda (e) (warn! R "screenshot ~a failed: ~a" rel (exn-message e)))])
    (define t1 (now-ms))
    (define png (screenshot-png (rstate-c st)))
    (emit! R rel png)
    (set-rstate-shots! st (add1 (rstate-shots st)))
    (set-run-screenshots! R (append (run-screenshots R) (list rel)))
    (log-info "~a: ~a bytes (photographed inside the DOM round while the panel was open, ~a ms)"
              rel (bytes-length png) (- (now-ms) t1))))

;; One poll of a running round: photographs the panel the extractor is holding open, or collects the result.
;; #t = the round has finished (capture in the state, or its error), #f = still running.
(define (round-step! R st)
  (cond
    [(rstate-finished st) #t]
    [else
     (define v (with-handlers ([exn:fail? (lambda (e) (hasheq 'fatal (exn-message e)))])
                 (cdp-eval (rstate-c st) SHOT-POLL-JS #:await #f #:timeout 120)))
     (cond
       [(not (hash? v))
        (set-rstate-error! st (format "poll returned ~s" v)) (set-rstate-finished! st #t) #t]
       [(hash-ref v 'fatal #f)
        (set-rstate-error! st (format "poll failed: ~a" (hash-ref v 'fatal))) (set-rstate-finished! st #t) #t]
       [else
        (define seq (int-or (jget v 'seq) 0))
        (define art (int-or (jget v 'article) -1))
        (define pan (let ([p (jget v 'panel)]) (and (string? p) (> (string-length p) 0) p)))
        (when (and (> seq 0) pan (>= art 0) (not (= seq (rstate-last-seq st))))
          (set-rstate-last-seq! st seq)
          (when (rstate-shots? st)
            (take-panel-shot! R st art pan)
            (phase! (format "Photographing the expanded ~a panel of message ~a…" pan art)
                    (min 0.78 (+ 0.44 (* 0.02 (rstate-shots st))))))
          (with-handlers ([exn:fail? (lambda (e) (warn! R "DOM round ~a: could not release the extractor after the ~a panel of turn ~a: ~a"
                                                       (rstate-k st) pan art (exn-message e)))])
            (cdp-eval (rstate-c st) (format "(() => { window.__grokShotDone = ~a; return true; })()" seq)
                      #:await #f #:timeout 120)))
        (cond
          [(eq? (jget v 'done) #t)
           (set-rstate-finished! st #t)
           (define e (jget v 'error))
           (cond
             [(string? e) (set-rstate-error! st (format "extractor threw: ~a" e))]
             [else
              (define cap (with-handlers ([exn:fail? (lambda (ex) (set-rstate-error! st (exn-message ex)) #f)])
                            (cdp-eval (rstate-c st) SHOT-RESULT-JS #:await #f #:timeout 600)))
              (if (hash? cap)
                  (begin (set-rstate-capture! st cap) (set-rstate-ok! st #t))
                  (unless (rstate-error st) (set-rstate-error! st "the extractor returned no capture")))])
           #t]
          [else #f])])]))

(define (round-run! R st)
  (let loop ()
    (unless (round-step! R st)
      (sleep (/ SHOT-POLL-MS 1000.0))
      (loop))))

;; Does this tab get frames?  Page.captureScreenshot on a tab the compositor is not drawing has to force one,
;; which was measured at 1.8-4.0 s per shot against 0.1-0.3 s on a drawn tab (12 panels: 22 s against 2.5 s).
;; Only one tab of a window is drawn, so the round that carries the screenshots is given to that one.
(define (tab-renders? c)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (eq? #t (cdp-eval c (string-append
                         "(async () => await Promise.race([new Promise((r) => requestAnimationFrame(() => r(true))),"
                         " new Promise((r) => setTimeout(() => r(false), 500))]))()")
                      #:timeout 15))))

;; Fresh navigation for one round; returns the page URL the poll settled on.
(define (round-navigate! R c url k attempt)
  (define n-rounds (max 1 (hash-ref (run-opts R) 'rounds)))
  (phase! (format "DOM pass ~a of ~a~a: loading the conversation…" k n-rounds
                  (if (> attempt 1) (format " (attempt ~a)" attempt) ""))
          (+ 0.34 (* 0.06 (/ (- k 1.0) n-rounds))))
  (log-info "DOM round ~a~a: navigating fresh to ~a" k (if (> attempt 1) (format " (attempt ~a)" attempt) "") url)
  (unless (cdp-navigate c url #:load-timeout 60)
    (warn! R "DOM round ~a: Page.loadEventFired did not arrive within 60 s; polling anyway" k))
  (define-values (href n) (wait-for-conversation R c))
  href)

;; Sends Page.navigate and returns at once, so the second tab's page loads while the first tab's is waited for.
(define (round-navigate-async! R c url k)
  (log-info "DOM round ~a: navigating fresh to ~a (second tab, loading in parallel)" k url)
  (with-handlers ([exn:fail? (lambda (e) (warn! R "DOM round ~a: Page.navigate failed: ~a" k (exn-message e)))])
    (cdp-navigate c url #:load-timeout 0)))

;; Waits for an already-navigating tab to render the conversation.
(define (round-wait! R c)
  (define-values (href n) (wait-for-conversation R c))
  href)

;; grok.com sometimes renders X posts without their hydration data (hydrate.rkt).  #t = accept the capture,
;; #f = it was rejected and the round has to be redone from a fresh navigation.
(define (round-accept? R st attempt)
  (define reason (capture-unhydrated? (rstate-capture st)))
  (cond
    [(and reason (< attempt DOM-HYDRATION-ATTEMPTS))
     (emit-string! R (format "raw/dom-capture-round~a.attempt~a-unhydrated.json" (rstate-k st) attempt)
                   (jsexpr->canonical-string (rstate-capture st)))
     (warn! R "DOM round ~a attempt ~a/~a: ~a; re-doing the round in ~a s"
            (rstate-k st) attempt DOM-HYDRATION-ATTEMPTS reason DOM-HYDRATION-DELAY-S)
     #f]
    [reason
     (warn! R "DOM round ~a: ~a after ~a attempt(s); keeping the capture as rendered" (rstate-k st) reason attempt)
     #t]
    [else #t]))

;; Everything a finished, accepted round owes the export directory.
(define (round-emit! R st href t0)
  (define k (rstate-k st))
  (define c (rstate-c st))
  (define cap (rstate-capture st))
  (define ext-ms (- (now-ms) (rstate-t0 st)))
  (define rel (format "raw/dom-capture-round~a.json" k))
  (emit-string! R rel (jsexpr->canonical-string cap))
  (define summary (capture-summary cap))
  (log-info "DOM round ~a: extractor finished in ~a ms; ~a articles, ~a thoughts panels, ~a sources panels, ~a thought rows, ~a source rows, remainingCollapsed ~a, tab ~a~a"
            k ext-ms (jget summary 'articles) (jget summary 'thoughtsPanels) (jget summary 'sourcesPanels)
            (jget summary 'thoughtRows) (jget summary 'sourceRows) (jget summary 'remainingCollapsedTotal)
            (jget summary 'visibilityState)
            (if (rstate-shots? st) (format ", ~a panel screenshots taken inside the round" (rstate-shots st)) ""))
  (for ([w (or-empty-list (jget summary 'extractorWarnings))]) (warn! R "DOM round ~a: extractor warning: ~a" k w))
  ;; group-chip helper (legacy citation enrichment evidence), kept per round
  (define chips
    (with-handlers ([exn:fail? (lambda (e) (warn! R "DOM round ~a: citation chip helper failed: ~a" k (exn-message e)) 'null)])
      (cdp-eval c CHIP-LINKS-JS #:timeout 120)))
  (when (hash? chips)
    (emit-string! R (format "raw/citation-chips-round~a.json" k) (jsexpr->canonical-string chips))
    (when (= k 1) (set-run-chip-links! R chips))
    (define n-groups (for/sum ([(key v) (in-hash chips)]) (if (list? v) (length v) 0)))
    (log-info "DOM round ~a: citation chip helper: ~a group chip(s) expanded (~a ms)" k n-groups (jget chips 'elapsedMs)))
  ;; page HTML after all expansion
  (with-handlers ([exn:fail? (lambda (e) (warn! R "DOM round ~a: page HTML capture failed: ~a" k (exn-message e)))])
    (define html (cdp-eval c "document.documentElement.outerHTML" #:await #f #:timeout 180))
    (define bs (string->bytes/utf-8 (jstr html)))
    (emit! R (format "raw/page-round~a.html" k) bs)
    (log-info "DOM round ~a: raw/page-round~a.html ~a bytes" k k (bytes-length bs)))
  (set-run-dom-rounds! R (append (run-dom-rounds R)
                                 (list (let ([h (make-hasheq)])
                                         (hash-set! h 'round k)
                                         (hash-set! h 'file rel)
                                         (hash-set! h 'finalUrl href)
                                         (hash-set! h 'extractorMs ext-ms)
                                         (hash-set! h 'roundMs (- (now-ms) t0))
                                         (when (rstate-shots? st) (hash-set! h 'screenshotsTaken (rstate-shots st)))
                                         (hash-set! h 'stats summary)
                                         (hash-set! h 'capture cap)
                                         h))))
  (timing! R (format "dom-round~a" k) t0)
  cap)

;; One round, start to finish, on its own connection (used for a redo and for rounds 3..N).
(define (round-sequential! R c url k shots?)
  (let attempt-loop ([attempt 1])
    (define t0 (now-ms))
    (define href (round-navigate! R c url k attempt))
    (define st (make-round c k shots?))
    (phase! (format "DOM pass ~a of ~a: expanding every Thoughts and Sources panel…" k (max 1 (hash-ref (run-opts R) 'rounds)))
            (+ 0.36 (* 0.06 (/ (- k 1.0) (max 1 (hash-ref (run-opts R) 'rounds))))))
    (extractor-start! st)
    (round-run! R st)
    (cond
      [(not (rstate-ok st)) (warn! R "DOM round ~a failed: ~a" k (or (rstate-error st) "no capture")) #f]
      [(not (round-accept? R st attempt)) (sleep DOM-HYDRATION-DELAY-S) (attempt-loop (add1 attempt))]
      [else (round-emit! R st href t0) st])))

;; Per-turn panel screenshots (SPEC 4), the fallback path: re-run the extractor with opts.only for one article and one
;; panel (it expands the panel and keeps it open), then capture the viewport.  Panels the round already photographed
;; are skipped, so on a healthy run this does nothing at all.
(define (dom-screenshots! R c cap)
  (define t0 (now-ms))
  (define arts (or-empty-list (jget cap 'articles)))
  (define n-shots 0)
  (define have (run-screenshots R))
  (for ([a (in-list arts)])
    (define i (jget a 'index))
    (when (and (exact-integer? i) (equal? (jget a 'role) "assistant"))
      (for ([pan (in-list '("thoughts" "sources"))])
        (define key (if (equal? pan "thoughts") 'thoughtsByArticle 'sourcesByArticle))
        (define entry (jget (or-empty-hash (jget cap key)) (string->symbol (number->string i))))
        (define rel (format "screenshots/turn~a-~a.png" i pan))
        (cond
          [(member rel have) (void)]                            ; already taken inside the round
          [(not (truthy? entry)) (log-info "screenshot turn~a-~a: skipped (article has no ~a panel)" i pan pan)]
          [else
           (with-handlers ([exn:fail? (lambda (e) (warn! R "screenshot ~a failed: ~a" rel (exn-message e)))])
             (define t1 (now-ms))
             (define r (run-extractor c (hasheq 'only (hasheq 'article i 'panel pan) 'settleMs 700 'maxPasses 25)))
             (define inline? (equal? (jget (or-empty-hash entry) 'layout) "canvas"))   ; agent replies: steps shown inline
             (define open? (if inline? "inline" (cdp-eval c "(() => { const a = document.querySelector('aside'); return a ? a.innerText.split('\\n')[0].trim() : null; })()" #:await #f)))
             (unless (or inline? (and (string? open?) (string-ci=? open? (if (equal? pan "thoughts") "Thoughts" "Sources"))))
               (error 'screenshot "panel ~a for article ~a is not open (aside header ~s)" pan i open?))
             (define rc (jget (or-empty-hash (jget (or-empty-hash (jget r key)) (string->symbol (number->string i)))) 'remainingCollapsed))
             (define png (screenshot-png c))
             (emit! R rel png)
             (set! n-shots (add1 n-shots))
             (set-run-screenshots! R (append (run-screenshots R) (list rel)))
             (log-info "~a: ~a bytes (re-opened after the round, remainingCollapsed ~a, ~a ms)" rel (bytes-length png) rc (- (now-ms) t1)))]))))
  (when (> n-shots 0) (with-handlers ([exn:fail? void]) (cdp-eval c CLOSE-ASIDE-JS #:await #f)))
  (timing! R "screenshots-fallback" t0)
  n-shots)

;; How many panels the extractor opened in a capture (= how many screenshots the round owed).
(define (panels-in-capture cap)
  (for*/sum ([key (in-list '(thoughtsByArticle sourcesByArticle))]
             [(k v) (in-hash (or-empty-hash (jget cap key)))])
    (if (truthy? v) 1 0)))

;; Everything round 1 owes once its capture is in hand.
(define (after-round1! R c st n-shots)
  (define cap (rstate-capture st))
  (define owed (panels-in-capture cap))
  (log-info "DOM round 1: ~a of ~a panels photographed inside a round" n-shots owed)
  (when (< n-shots owed)
    (define n (with-handlers ([exn:fail? (lambda (e) (warn! R "per-turn screenshot fallback failed: ~a" (exn-message e)) 0)])
                (dom-screenshots! R c cap)))
    (log-info "per-turn screenshot fallback: ~a panel(s) re-expanded" n)))

;; The whole DOM lane: rounds 1 and 2 concurrently in two tabs, then rounds 3..N.
(define (dom-lane! R c url port)
  (define n-rounds (max 1 (hash-ref (run-opts R) 'rounds)))
  (define-values (c2 target2)
    (if (>= n-rounds 2)
        (with-handlers ([exn:fail? (lambda (e)
                                     (warn! R "second tab for the concurrent DOM round could not be opened (~a); the rounds run one after the other"
                                            (exn-message e))
                                     (values #f #f))])
          (define t (cdp-new-target "127.0.0.1" port "about:blank"))
          (define cc (cdp-connect (jstr (jget t 'webSocketDebuggerUrl))))
          (cdp-call cc "Page.enable")
          (cdp-call cc "Runtime.enable")
          (cdp-call cc "Network.enable")
          (set-viewport! R cc)
          (log-info "created tab ~a for DOM round 2 (it runs at the same time as round 1)" (jstr (jget t 'id)))
          (values cc (jstr (jget t 'id))))
        (values #f #f)))
  (dynamic-wind
   void
   (lambda ()
     (define next-round
       (cond
         [c2
          (define t0 (now-ms))
          (log-info "DOM lane: rounds 1 and 2 run at the same time in two tabs")
          (round-navigate-async! R c2 url 2)          ; tab 2 loads while tab 1 is loaded and waited for
          (define href1 (round-navigate! R c url 1 1))
          (define href2 (round-wait! R c2))
          (define st1 (make-round c 1 #f))
          (define st2 (make-round c2 2 #f))
          (define draws1 (tab-renders? c))
          (define draws2 (tab-renders? c2))
          ;; the tab created last is the active one, so it is the one the compositor draws; fall back to
          ;; tab 1 only when tab 2 does not draw while tab 1 does
          (if (or draws2 (not draws1)) (set-rstate-shots?! st2 #t) (set-rstate-shots?! st1 #t))
          (log-info "tab drawing: round 1 ~a, round 2 ~a; the panel screenshots are taken in round ~a's tab"
                    (if draws1 "yes" "no") (if draws2 "yes" "no") (if (rstate-shots? st1) 1 2))
          (phase! "Expanding every Thoughts and Sources panel in both tabs…" 0.40)
          (extractor-start! st1)
          (extractor-start! st2)
          (let loop ()
            (unless (and (rstate-finished st1) (rstate-finished st2))
              (define a (round-step! R st1))
              (define b (round-step! R st2))
              (unless (and a b) (sleep (/ SHOT-POLL-MS 1000.0)))
              (loop)))
          (timing! R "dom-rounds-1-and-2-concurrent" t0)
          ;; round 1 first: the transcript writers, the verifier and the citation probe all use it
          (cond
            [(not (rstate-ok st1)) (warn! R "DOM round 1 failed: ~a" (or (rstate-error st1) "no capture"))]
            [(round-accept? R st1 1) (round-emit! R st1 href1 t0) (after-round1! R c st1 (+ (rstate-shots st1) (rstate-shots st2)))]
            ;; rejected as unhydrated: the redo carries the screenshots only if the rejected round was the one
            ;; that took them (they came from a page rendered from a stripped payload and must be retaken)
            [else (sleep DOM-HYDRATION-DELAY-S)
                  (define st (round-sequential! R c url 1 (rstate-shots? st1)))
                  (when st (after-round1! R c st (+ (rstate-shots st) (if (rstate-shots? st1) 0 (rstate-shots st2)))))])
          (cond
            [(not (rstate-ok st2)) (warn! R "DOM round 2 failed: ~a" (or (rstate-error st2) "no capture"))]
            [(round-accept? R st2 1) (round-emit! R st2 href2 t0)]
            [else (sleep DOM-HYDRATION-DELAY-S) (round-sequential! R c2 url 2 (rstate-shots? st2))])
          3]
         [else
          (define st (round-sequential! R c url 1 #t))
          (when st (after-round1! R c st (rstate-shots st)))
          2]))
     (for ([k (in-range next-round (add1 n-rounds))])
       (round-sequential! R c url k #f)))
   (lambda ()
     (when c2
       (with-handlers ([exn:fail? (lambda (e) (warn! R "closing tab ~a failed: ~a" target2 (exn-message e)))])
         (cdp-close c2)
         (unless (hash-ref (run-opts R) 'keep-open)
           (define-values (code body) (cdp-close-target "127.0.0.1" port target2))
           (log-info "closed tab ~a (HTTP ~a)" target2 code)))))))

;; ------------------------------------------------------------------ verification assembly
(define (dom-only-checks R)
  ;; expansion-complete on round 1 + stability across rounds (no transcript available)
  (define rounds (run-dom-rounds R))
  (cond
    [(null? rounds) '()]
    [else
     (define cap1 (hash-ref (car rounds) 'capture))
     (define exp
       (for*/list ([pair (in-list (list (cons 'thoughtsByArticle "thoughts") (cons 'sourcesByArticle "sources")))]
                   [(k pan) (in-hash (or-empty-hash (jget cap1 (car pair))))]
                   #:unless (eq? pan 'null))
         (define rc (jget pan 'remainingCollapsed))
         (hasheq 'name "expansion-complete" 'turnIndex (string->number (symbol->string k))
                 'expected (hasheq 'panel (cdr pair) 'remainingCollapsed 0)
                 'actual (hasheq 'panel (cdr pair) 'remainingCollapsed rc 'rows (jget pan 'rowCount) 'links (jget pan 'linkCount))
                 'ok (equal? rc 0))))
     (append (sort exp < #:key (lambda (c) (hash-ref c 'turnIndex))) (stability-checks R))]))

(define (stability-checks R)
  (define rounds (run-dom-rounds R))
  (if (or (null? rounds) (null? (cdr rounds)))
      '()
      (for/list ([d (cdr rounds)])
        (stability-check (hash-ref (car rounds) 'capture) (hash-ref d 'capture)
                         (hash-ref (car rounds) 'file) (hash-ref d 'file)))))

(define (assemble-verification R transcript t-chunk t-legacy)
  (define opts (run-opts R))
  (define rounds (run-dom-rounds R))
  (define tool (format "~a ~a" TOOL-NAME TOOL-VERSION))
  (define api-status
    (cond [(hash-ref opts 'skip-api) "skipped (--skip-api)"]
          [(not transcript) "unavailable"]
          [else "ok"]))
  (define dom-status
    (cond [(hash-ref opts 'skip-dom) "skipped (--skip-dom)"]
          [(null? rounds) "unavailable"]
          [else (format "ok (~a round~a)" (length rounds) (if (= (length rounds) 1) "" "s"))]))
  (define consistency
    (if (equal? (run-api-lane R) "share")
        (list (api-consistency-check t-chunk t-legacy))
        '()))
  (cond
    [(and transcript (pair? rounds))
     (verify-capture transcript (hash-ref (car rounds) 'capture)
                     #:attachments-dir (build-path (run-export-dir R) "attachments")
                     #:transcript-name "transcript.json"
                     #:capture-name (hash-ref (car rounds) 'file)
                     #:tool tool
                     #:extra-checks (append (stability-checks R) consistency)
                     #:extra-fields (list (cons 'api api-status) (cons 'dom dom-status)
                                          (cons 'rounds (length rounds))))]
    [transcript
     ;; DOM lane skipped or failed: only the API-side check(s) can run
     (define checks consistency)
     (define failed (filter (lambda (c) (not (hash-ref c 'ok))) checks))
     (define ok (and (null? failed) (hash-ref opts 'skip-dom)))
     (hasheq 'tool tool 'transcript "transcript.json" 'capture 'null
             'api api-status 'dom dom-status
             'checks checks
             'byName (for/hasheq ([c checks]) (values (string->symbol (hash-ref c 'name)) (hasheq 'total 1 'ok (if (hash-ref c 'ok) 1 0) 'failed (if (hash-ref c 'ok) 0 1))))
             'summary (hasheq 'total (length checks) 'ok (- (length checks) (length failed)) 'failed (length failed))
             'failedChecks (for/list ([c failed]) (hasheq 'name (hash-ref c 'name) 'turnIndex (hash-ref c 'turnIndex)))
             'warnings (if (hash-ref opts 'skip-dom)
                           (list "DOM lane skipped (--skip-dom): SPEC 5 checks 1-12 not performed")
                           (list "DOM lane produced no capture: SPEC 5 checks 1-12 not performed"))
             'note "the page content was not verified against the API export"
             'ok ok)]
    [else
     ;; no transcript: API unavailable or skipped -> DOM-only report, never ok
     (define checks (dom-only-checks R))
     (define failed (filter (lambda (c) (not (hash-ref c 'ok))) checks))
     (hasheq 'tool tool 'transcript 'null 'capture (if (pair? rounds) (hash-ref (car rounds) 'file) 'null)
             'api api-status 'dom dom-status
             'checks checks
             'byName (for/fold ([h (hasheq)]) ([c checks])
                       (define k (string->symbol (hash-ref c 'name)))
                       (define b (hash-ref h k (hasheq 'total 0 'ok 0 'failed 0)))
                       (hash-set h k (hash-set (hash-update b 'total add1) (if (hash-ref c 'ok) 'ok 'failed) (add1 (hash-ref b (if (hash-ref c 'ok) 'ok 'failed))))))
             'summary (hasheq 'total (length checks) 'ok (- (length checks) (length failed)) 'failed (length failed))
             'failedChecks (for/list ([c failed]) (hasheq 'name (hash-ref c 'name) 'turnIndex (hash-ref c 'turnIndex)))
             'warnings (list (format "api: ~a -- no transcript could be built, SPEC 5 checks 1-10 and 13 not performed; only the DOM lane was exported" api-status))
             'note "export is DOM-only; the conversation content could not be cross-verified against the API"
             'ok #f)]))

;; ------------------------------------------------------------------ Gemini lane
;; gemini.google.com/app/<id>: API lane = the page's hNvQHb batchexecute RPC, DOM lane = scroll to the
;; very top until nothing more loads, then compare turn counts and texts against the API transcript.
(define (run-gemini R url)
  (define opts (run-opts R))
  (define port (hash-ref opts 'port))
  (define cid (gemini-conv-id url))
  (define t-cdp (now-ms))
  (phase! (format "Connecting to Chrome on port ~a…" port) 0.02)
  (define ver (ensure-cdp! port (hash-ref opts 'chrome) (hash-ref opts 'profile) (hash-ref opts 'no-launch)))
  (log-info "DevTools: ~a on port ~a" (jget ver 'Browser) port)
  (define target (cdp-new-target "127.0.0.1" port "about:blank"))
  (define target-id (jstr (jget target 'id)))
  (report! 'tab (list "127.0.0.1" port target-id))
  (define c (cdp-connect (jstr (jget target 'webSocketDebuggerUrl))))
  (timing! R "connect" t-cdp)
  (define exit-code 1)
  (define (fail-safe thunk) (with-handlers ([exn:fail? (lambda (e) (warn! R "~a" (exn-message e)))]) (thunk)))
  (dynamic-wind
   void
   (lambda ()
     (with-handlers ([exn:fail?
                      (lambda (e)
                        (log-info "ERROR: ~a" (exn-message e))
                        (unless (run-export-dir R)
                          (fail-safe (lambda () (make-export-dir! R (string-append "gemini-" (or cid "unknown"))))))
                        (when (run-export-dir R)
                          (fail-safe (lambda () (flush-pending! R)))
                          (fail-safe (lambda () (write-manifest! R 1 #:error (exn-message e)))))
                        (set! exit-code 1))])
       (cdp-call c "Page.enable")
       (cdp-call c "Runtime.enable")
       (set-viewport! R c)
       (define t-nav (now-ms))
       (phase! "Loading the Gemini conversation…" 0.08)
       (log-info "Navigating to ~a" url)
       (unless (cdp-navigate c url #:load-timeout 60)
         (warn! R "Page.loadEventFired did not arrive within 60 s; polling anyway"))
       ;; wait until the tab is signed in and shows the conversation (this is where you sign in to Google)
       (define deadline (+ (now-ms) (* 1000 (hash-ref opts 'timeout))))
       (define state
         (let loop ([told? #f])
           (define s (with-handlers ([exn:fail? (lambda (e) (hasheq))]) (cdp-eval c GEMINI-STATE-JS #:await #f)))
           (define host (jget s 'host))
           (cond
             [(and (hash? s) (equal? host "gemini.google.com") (eq? (jget s 'signedIn) #t)
                   (exact-positive-integer? (jget s 'queries)))
              s]
             ;; Google's "unusual traffic" interstitial is a CAPTCHA for a person; waiting on it only adds traffic
             [(and (string? (jget s 'href)) (regexp-match? #rx"^https?://(www\\.)?google\\.[a-z.]+/sorry" (jget s 'href)))
              (error 'gemini "Google is showing its unusual-traffic page for this browser (~a); open gemini.google.com in the exporter's Chrome window, clear it there, and export again later" (jget s 'href))]
             [(> (now-ms) deadline)
              (error 'gemini "the conversation did not appear within ~a s (last page: ~a)" (hash-ref opts 'timeout) (jget s 'href))]
             [else
              (when (and (not told?) (string? host) (not (equal? host "gemini.google.com")))
                (phase! "Sign in to Google in the Chrome window that opened…" 0.09)
                (log-info "Waiting for a Google sign-in in the Chrome window (profile ~a)" (hash-ref opts 'profile)))
              (sleep 2)
              (loop (or told? (and (string? host) (not (equal? host "gemini.google.com")))))])))
       (timing! R "navigate-and-wait" t-nav)
       (define final-url (jstr (jget state 'href)))
       (set-run-final-url! R final-url)
       (set-run-conversation-id! R (string-append "c_" cid))
       (make-export-dir! R (string-append "gemini-" cid))
       ;; API lane
       (define t-api (now-ms))
       (phase! "Fetching the conversation from Gemini…" 0.14)
       (define fetched
         (if (hash-ref opts 'skip-api)
             (hasheq 'pages '() 'errors '("--skip-api") 'title 'null)
             (cdp-run-async-function c GEMINI-FETCH-JS (hasheq 'cid cid 'pageSize 1000) #:timeout 900)))
       (for ([e (or-empty-list (jget fetched 'errors))]) (warn! R "Gemini API: ~a" e))
       (define payloads
         (for/list ([pg (or-empty-list (jget fetched 'pages))] [i (in-naturals 1)])
           (define raw (jstr (jget pg 'text)))
           (define rel (format "raw/gemini-hNvQHb-page~a.txt" i))
           (define bs (string->bytes/utf-8 raw))
           (emit! R rel bs)
           (set-run-raws! R (append (run-raws R)
                                    (list (hasheq 'name "hNvQHb" 'url "/_/BardChatUi/data/batchexecute?rpcids=hNvQHb"
                                                  'status (jget pg 'status) 'bytes (bytes-length bs) 'file rel))))
           (or (parse-batchexecute raw)
               (begin (warn! R "~a: no hNvQHb payload could be parsed" rel) #f))))
       (define good (filter values payloads))
       (timing! R "api" t-api)
       (define title (let ([v (jget fetched 'title)]) (and (string? v) v)))
       (define transcript (and (pair? good) (gemini-payload->transcript good final-url title)))
       (set-run-api-lane! R (cond [(hash-ref opts 'skip-api) "skipped"] [transcript "gemini-batchexecute"] [else "unavailable"]))
       (set-run-parsed-format! R "gemini-hNvQHb")
       (when transcript
         (emit-string! R "raw/transcript-api.json" (jsexpr->canonical-string transcript))
         (write-transcript-outputs! R transcript #:html #f)
         (log-info "Gemini API: ~a page(s), ~a turns" (length good) (length (or-empty-list (jget transcript 'turns)))))
       ;; DOM lane: scroll up until nothing more loads
       (define dom
         (cond
           [(hash-ref opts 'skip-dom) (log-info "DOM lane skipped (--skip-dom)") #f]
           [else
            (phase! "Scrolling to the top of the Gemini chat until every turn has loaded…" 0.4)
            (define t-dom (now-ms))
            (define d (cdp-run-async-function c GEMINI-DOM-JS (hasheq 'waitMs 1000 'stableRounds 8 'maxMs 900000) #:timeout 1000))
            (emit-string! R "raw/dom-gemini.json" (jsexpr->canonical-string d))
            (timing! R "dom" t-dom)
            (log-info "Gemini DOM: ~a user turns, ~a replies after ~a scroll rounds (~a ms)"
                      (length (or-empty-list (jget d 'queries))) (length (or-empty-list (jget d 'responses)))
                      (jget d 'rounds) (jget d 'ms))
            d]))
       (phase! "Capturing the page and the screenshot…" 0.8)
       (capture-page! R c)
       ;; verification
       (define-values (checks vwarns)
         (if (and transcript dom) (gemini-dom-checks transcript dom) (values '() '())))
       (define failed (filter (lambda (k) (not (eq? (jget k 'ok) #t))) checks))
       (define verification
         (hasheq 'tool TOOL-NAME
                 'api (if transcript "ok" "unavailable")
                 'dom (if dom "ok (1 round)" "skipped")
                 'capture (if dom "raw/dom-gemini.json" 'null)
                 'transcript (if transcript "transcript.json" 'null)
                 'rounds (if dom 1 0)
                 'checks checks
                 'failedChecks (for/list ([k failed]) (hasheq 'name (jget k 'name) 'turnIndex (jget k 'turnIndex)))
                 'byName (for/hasheq ([nm (remove-duplicates (map (lambda (k) (jget k 'name)) checks))])
                           (values (string->symbol nm)
                                   (hasheq 'total (count (lambda (k) (equal? (jget k 'name) nm)) checks)
                                           'failed (count (lambda (k) (equal? (jget k 'name) nm)) failed))))
                 'summary (hasheq 'total (length checks) 'ok (- (length checks) (length failed)) 'failed (length failed))
                 'warnings vwarns
                 'ok (and transcript dom (null? failed) #t)))
       (set-run-verification! R verification)
       (emit-string! R "verification.json" (jsexpr->canonical-string verification))
       (log-info "verification.json: ~a checks, ~a failed" (length checks) (length failed))
       (for ([f failed]) (log-info "  FAILED ~a turn ~a: ~a" (jget f 'name) (jget f 'turnIndex) (jget f 'detail)))
       (when transcript
         (phase! "Writing transcript.html…" 0.95)
         (write-transcript-outputs! R transcript #:html #t))
       (define behavior-ok? (if transcript (write-behavior-reports! R transcript) #t))
       (set-run-transport! R (cdp-transport-stats c))
       (set! exit-code (cond [(and transcript (jget verification 'ok) behavior-ok?) 0]
                             [transcript 2]
                             [else 1]))
       (write-manifest! R exit-code
                        #:error (if (= exit-code 1) "the Gemini API returned no parsable conversation" #f))))
   (lambda ()
     (unless (hash-ref opts 'keep-open)
       (with-handlers ([exn:fail? (lambda (e) (warn! R "closing tab ~a failed: ~a" target-id (exn-message e)))])
         (cdp-close c)
         (cdp-close-target "127.0.0.1" port target-id)))))
  (phase! "Finished." 1.0)
  (log-info "exit code  : ~a" exit-code)
  (when (run-export-dir R) (log-info "EXPORT_DIR=~a" (path->string (run-export-dir R))))
  exit-code)

;; ------------------------------------------------------------------ Qwen lane
(define QWEN-EXE "C:\\Program Files\\Qwen\\Qwen.exe")

(define (powershell! cmd)
  (system* (or (find-executable-path "powershell.exe") "C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\powershell.exe")
           "-NoProfile" "-NonInteractive" "-Command" cmd))

(define (qwen-webview-target)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (for/first ([t (cdp-list-targets "127.0.0.1" QWEN-DEBUG-PORT)]
                #:when (and (member (jget t 'type) '("webview" "page"))
                            (regexp-match? #rx"^https://chat[.]qwen[.]ai" (jstr (jget t 'url)))))
      t)))

;; The Qwen desktop app, reachable on QWEN-DEBUG-PORT.  A running Qwen is never closed or restarted: a
;; person or another agent (Codex drove the same window on 2026-09-14) may be using it.  Only when Qwen is
;; not running at all is it started, with the port.
(define (ensure-qwen-app! R)
  (or (qwen-webview-target)
      (let ()
        (unless (file-exists? QWEN-EXE)
          (error 'qwen "the Qwen desktop app is not installed at ~a" QWEN-EXE))
        (define running? (powershell! "if (Get-Process Qwen -ErrorAction SilentlyContinue) { exit 0 } else { exit 1 }"))
        (when running?
          (error 'qwen "Qwen is running without --remote-debugging-port=~a, so its session cannot be read; it is not restarted automatically because someone may be using it" QWEN-DEBUG-PORT))
        (phase! "Starting the Qwen app…" 0.05)
        (powershell! (format "Start-Process -FilePath '~a' -ArgumentList '--remote-debugging-port=~a'" QWEN-EXE QWEN-DEBUG-PORT))
        (let loop ([n 0])
          (define t (qwen-webview-target))
          (cond [t t]
                [(> n 90) (error 'qwen "the Qwen app did not expose chat.qwen.ai on port ~a within 90 s" QWEN-DEBUG-PORT)]
                [else (sleep 1) (loop (add1 n))])))))

;; A browser the exporter owns for Qwen: Chrome inside WSL Ubuntu (started with --remote-debugging-port=9333,
;; profile ~/.config/exporter-chrome), reachable from Windows through WSL localhost forwarding.  Its own tab can
;; be navigated and scrolled freely without touching any Windows window a person or another agent is using.
;; 2026-09-14: WSLg showed the Linux Chrome window blank on this PC, so nobody could sign in there.  The
;; exporter's own Windows Chrome (port 9222, profile GrokExportClaude\profile) is signed in to chat.qwen.ai
;; and is tried first; its tabs are the exporter's, not the Qwen app another agent drives.
(define QWEN-OWNED-PORTS '(9222 9333))

;; -> (list cdp target-id port) of a new tab on chat-url that is signed in to chat.qwen.ai, or #f
(define (qwen-owned-tab R chat-url)
  (for/or ([port QWEN-OWNED-PORTS]) (qwen-owned-tab-on R chat-url port)))

(define (qwen-owned-tab-on R chat-url QWEN-OWNED-PORT)
  (with-handlers ([exn:fail? (lambda (e) (log-info "owned browser on port ~a unavailable: ~a" QWEN-OWNED-PORT (exn-message e)) #f)])
    (cdp-list-targets "127.0.0.1" QWEN-OWNED-PORT)
    (define t (cdp-new-target "127.0.0.1" QWEN-OWNED-PORT chat-url))
    (define tid (jstr (jget t 'id)))
    (define oc (cdp-connect (jstr (jget t 'webSocketDebuggerUrl))))
    (cdp-call oc "Runtime.enable")
    (let loop ([n 0])
      (define s (with-handlers ([exn:fail? (lambda (e) (hasheq))]) (cdp-eval oc QWEN-STATE-JS #:await #f)))
      (cond
        [(and (eq? (jget s 'signedIn) #t) (equal? (jget s 'host) "chat.qwen.ai")
              (exact-positive-integer? (jget s 'assistants)))
         (log-info "owned browser (port ~a): signed in, chat rendered" QWEN-OWNED-PORT)
         (list oc tid QWEN-OWNED-PORT)]
        [(> n 40)
         (log-info "owned browser (port ~a): ~a" QWEN-OWNED-PORT
                   (if (eq? (jget s 'signedIn) #t) "chat did not render within 40 s" "not signed in to chat.qwen.ai"))
         (with-handlers ([exn:fail? void]) (cdp-close oc))
         (with-handlers ([exn:fail? void]) (cdp-close-target "127.0.0.1" QWEN-OWNED-PORT tid))
         #f]
        [else (sleep 1) (loop (add1 n))]))))

(define (run-qwen R input)
  (define opts (run-opts R))
  (define t-cdp (now-ms))
  (phase! "Connecting to Qwen…" 0.02)
  (define input-id (qwen-chat-id input))
  ;; the Qwen app: needed for "qwen" (which chat is open) and as the fallback session; only read
  (define app-target
    (with-handlers ([exn:fail? (lambda (e) (log-info "Qwen app: ~a" (exn-message e)) #f)])
      (if input-id (qwen-webview-target) (ensure-qwen-app! R))))
  (define app-c (and app-target (cdp-connect (jstr (jget app-target 'webSocketDebuggerUrl)))))
  (define app-state
    (if app-c (with-handlers ([exn:fail? (lambda (e) (hasheq))]) (cdp-eval app-c QWEN-STATE-JS #:await #f)) (hasheq)))
  (define app-href (let ([h (jget app-state 'href)]) (and (string? h) h)))
  (define id (or input-id (and app-href (qwen-chat-id app-href))))
  (define chat-url (and id (format "https://chat.qwen.ai/c/~a" id)))
  (define owned (and chat-url (not (hash-ref opts 'skip-dom)) (qwen-owned-tab R chat-url)))
  (define c (or (and owned (car owned)) (and app-c (eq? (jget app-state 'signedIn) #t) app-c)))
  (timing! R "connect" t-cdp)
  (define exit-code 1)
  (define (fail-safe thunk) (with-handlers ([exn:fail? (lambda (e) (warn! R "~a" (exn-message e)))]) (thunk)))
  (dynamic-wind
   void
   (lambda ()
     (with-handlers ([exn:fail?
                      (lambda (e)
                        (log-info "ERROR: ~a" (exn-message e))
                        (unless (run-export-dir R)
                          (fail-safe (lambda () (make-export-dir! R (string-append "qwen-" (or id "unknown"))))))
                        (when (run-export-dir R)
                          (fail-safe (lambda () (flush-pending! R)))
                          (fail-safe (lambda () (write-manifest! R 1 #:error (exn-message e)))))
                        (set! exit-code 1))])
       (unless id
         (error 'qwen "no chat id in ~s: paste a https://chat.qwen.ai/c/<id> link, or open the chat in the Qwen app (it shows ~a)"
                input (or app-href "nothing reachable")))
       (unless c
         (error 'qwen "no signed-in Qwen session: neither an exporter browser on ports ~a nor the Qwen app on port ~a is signed in and reachable"
                QWEN-OWNED-PORTS QWEN-DEBUG-PORT))
       (log-info "Qwen session: ~a" (if owned (format "exporter-owned browser, port ~a" (caddr owned))
                                        (format "Qwen desktop app, port ~a (read-only)" QWEN-DEBUG-PORT)))
       (set-run-final-url! R chat-url)
       (set-run-conversation-id! R id)
       (make-export-dir! R (string-append "qwen-" id))
       ;; API lane
       (define t-api (now-ms))
       (phase! "Fetching the conversation from Qwen…" 0.12)
       (define fetched (cdp-run-async-function c QWEN-FETCH-JS (hasheq 'id id) #:timeout 300))
       (define raw (jstr (jget fetched 'text)))
       (define bs (string->bytes/utf-8 raw))
       (emit! R "raw/qwen-chat.json" bs)
       (set-run-raws! R (append (run-raws R) (list (hasheq 'name "chat" 'url (format "/api/v2/chats/~a" id)
                                                           'status (jget fetched 'status) 'bytes (bytes-length bs)
                                                           'file "raw/qwen-chat.json"))))
       (define payload (with-handlers ([exn:fail? (lambda (e) #f)]) (string->jsexpr raw)))
       (define ok-payload (and (hash? payload) (eq? (jget payload 'success) #t) payload))
       (unless ok-payload (warn! R "Qwen API: HTTP ~a, no usable payload (~a)" (jget fetched 'status) (substring raw 0 (min 200 (string-length raw)))))
       (timing! R "api" t-api)
       (define transcript (and ok-payload (qwen-payload->transcript ok-payload chat-url)))
       (set-run-api-lane! R (if transcript "qwen-api-v2" "unavailable"))
       (set-run-parsed-format! R "qwen-v2-chat")
       (when transcript
         (emit-string! R "raw/transcript-api.json" (jsexpr->canonical-string transcript))
         (write-transcript-outputs! R transcript #:html #f)
         (define blocked (for/list ([t (or-empty-list (jget transcript 'turns))] #:when (hash? (jget t 'error))) t))
         (log-info "Qwen API: ~a turns on the current branch, ~a messages in total, ~a with an error"
                   (length (or-empty-list (jget transcript 'turns)))
                   (jget (jget transcript 'conversation) 'messagesTotal) (length blocked))
         (for ([t blocked])
           (note! R "turn ~a: Qwen error ~a (stage ~a): ~a" (jget t 'index) (jget (jget t 'error) 'code)
                  (jget (jget t 'error) 'stage) (jget (jget t 'error) 'details))))
       ;; DOM lane.  Owned Linux tab: scroll to the top, then step through the whole chat collecting every
       ;; rendered message.  Otherwise the shared app window is only read as it is, never navigated or scrolled.
       (define dom
         (cond
           [(hash-ref opts 'skip-dom) (log-info "DOM lane skipped (--skip-dom)") #f]
           [owned
            (phase! "Scrolling through the whole Qwen chat in the exporter's own browser tab…" 0.4)
            (define t-dom (now-ms))
            (define d (cdp-run-async-function c QWEN-DOM-JS (hasheq 'scroll #t 'waitMs 1000 'stableRounds 8 'stepMs 700 'maxMs 900000) #:timeout 1000))
            (emit-string! R "raw/dom-qwen.json" (jsexpr->canonical-string d))
            (timing! R "dom" t-dom)
            d]
           [(not (equal? (and app-href (qwen-chat-id app-href)) id))
            (note! R "page check skipped: the Linux browser is not signed in and the Qwen app shows ~a (the app window is never switched)" app-href)
            #f]
           [else
            (phase! "Reading the chat as the Qwen window shows it (read-only)…" 0.4)
            (define d (cdp-run-async-function c QWEN-DOM-JS (hasheq 'scroll #f) #:timeout 120))
            (emit-string! R "raw/dom-qwen.json" (jsexpr->canonical-string d))
            d]))
       (when dom
         (log-info "Qwen DOM: ~a user messages, ~a replies seen (~a top rounds, ~a steps, ~a ms)"
                   (length (or-empty-list (jget dom 'users))) (length (or-empty-list (jget dom 'assistants)))
                   (jget dom 'rounds) (jget dom 'steps) (jget dom 'ms))
         (with-handlers ([exn:fail? (lambda (e) (warn! R "page HTML capture failed: ~a" (exn-message e)))])
           (emit-string! R "raw/page-initial.html" (jstr (cdp-eval c "document.documentElement.outerHTML" #:await #f #:timeout 180)))))
       ;; the full-page screenshot only in the owned tab (it resizes the page)
       (when owned (capture-page! R c))
       (define-values (checks vwarns)
         (if (and transcript dom) (qwen-dom-checks transcript dom) (values '() '())))
       (for ([w vwarns]) (log-info "  ! ~a" w))
       (define failed (filter (lambda (k) (not (eq? (jget k 'ok) #t))) checks))
       (define verification
         (hasheq 'tool TOOL-NAME 'api (if transcript "ok" "unavailable")
                 'dom (cond [owned "ok (exporter-owned browser, scrolled)"] [dom "ok (read-only snapshot of the Qwen app)"] [else "skipped"])
                 'capture (if dom "raw/dom-qwen.json" 'null) 'transcript (if transcript "transcript.json" 'null)
                 'rounds (if dom 1 0) 'checks checks
                 'failedChecks (for/list ([k failed]) (hasheq 'name (jget k 'name) 'turnIndex (jget k 'turnIndex)))
                 'byName (for/hasheq ([nm (remove-duplicates (map (lambda (k) (jget k 'name)) checks))])
                           (values (string->symbol nm)
                                   (hasheq 'total (count (lambda (k) (equal? (jget k 'name) nm)) checks)
                                           'failed (count (lambda (k) (equal? (jget k 'name) nm)) failed))))
                 'summary (hasheq 'total (length checks) 'ok (- (length checks) (length failed)) 'failed (length failed))
                 'warnings vwarns
                 'ok (and transcript dom (null? failed) #t)))
       (set-run-verification! R verification)
       (emit-string! R "verification.json" (jsexpr->canonical-string verification))
       (log-info "verification.json: ~a checks, ~a failed" (length checks) (length failed))
       (for ([f failed]) (log-info "  FAILED ~a turn ~a: ~a" (jget f 'name) (jget f 'turnIndex) (jget f 'detail)))
       (when transcript (write-transcript-outputs! R transcript #:html #t))
       (define behavior-ok? (if transcript (write-behavior-reports! R transcript) #t))
       (set-run-transport! R (cdp-transport-stats c))
       (set! exit-code (cond [(and transcript (jget verification 'ok) behavior-ok?) 0] [transcript 2] [else 1]))
       (write-manifest! R exit-code #:error (if (= exit-code 1) "the Qwen API returned no usable conversation" #f))))
   (lambda ()
     (when owned
       (with-handlers ([exn:fail? void]) (cdp-close (car owned)))
       (unless (hash-ref opts 'keep-open)
         (with-handlers ([exn:fail? void]) (cdp-close-target "127.0.0.1" (caddr owned) (cadr owned)))))
     (when app-c (with-handlers ([exn:fail? void]) (cdp-close app-c)))))
  (phase! "Finished." 1.0)
  (log-info "exit code  : ~a" exit-code)
  (when (run-export-dir R) (log-info "EXPORT_DIR=~a" (path->string (run-export-dir R))))
  exit-code)


;; ------------------------------------------------------------------ live mode
(define (run-live R)
  (define u (hash-ref (run-opts R) 'url))
  (cond
    [(gemini-url? u) (run-gemini R (string-trim u))]
    [(qwen-input? u) (run-qwen R (string-trim u))]
    [else (run-live-grok R)]))

(define (run-live-grok R)
  (define opts (run-opts R))
  (define url (normalize-url (hash-ref opts 'url)))
  (define port (hash-ref opts 'port))
  (define-values (kind0 id0) (classify-url url))
  (define t-cdp (now-ms))
  (phase! (format "Connecting to Chrome on port ~a…" port) 0.02)
  (define ver (ensure-cdp! port (hash-ref opts 'chrome) (hash-ref opts 'profile) (hash-ref opts 'no-launch)))
  (log-info "DevTools: ~a on port ~a" (jget ver 'Browser) port)
  (phase! "Opening a new browser tab…" 0.05)
  (define target (cdp-new-target "127.0.0.1" port "about:blank"))
  (define target-id (jstr (jget target 'id)))
  (define ws-url (jstr (jget target 'webSocketDebuggerUrl)))
  (log-info "Created tab ~a" target-id)
  (report! 'tab (list "127.0.0.1" port target-id))
  (define c (cdp-connect ws-url))
  (timing! R "connect" t-cdp)
  (define exit-code 1)
  (define transcript #f)
  (define t-chunk #f)
  (define t-legacy #f)
  (define (fail-safe thunk) (with-handlers ([exn:fail? (lambda (e) (warn! R "~a" (exn-message e)))]) (thunk)))
  (dynamic-wind
   void
   (lambda ()
     (with-handlers ([exn:fail?
                      (lambda (e)
                        (log-info "ERROR: ~a" (exn-message e))
                        (unless (run-export-dir R)
                          (fail-safe (lambda () (make-export-dir! R (or (run-conversation-id R) id0 "unknown")))))
                        (fail-safe (lambda () (set-run-transport! R (cdp-transport-stats c))))
                        (when (run-export-dir R)
                          (fail-safe (lambda () (flush-pending! R)))
                          (fail-safe (lambda () (write-manifest! R 1 #:error (exn-message e))))
                          (log-info "Raw material written to ~a" (path->string (run-export-dir R))))
                        (set! exit-code 1))])
       ;; step 2
       (cdp-call c "Page.enable")
       (cdp-call c "Runtime.enable")
       (cdp-call c "Network.enable")
       (set-viewport! R c)
       (define t-nav (now-ms))
       (phase! "Loading the conversation…" 0.08)
       (log-info "Navigating to ~a" url)
       (unless (cdp-navigate c url #:load-timeout 60)
         (warn! R "Page.loadEventFired did not arrive within 60 s; polling anyway"))
       (define-values (final-url n-articles) (wait-for-conversation R c))
       (timing! R "navigate-and-wait" t-nav)
       (set-run-final-url! R final-url)
       ;; step 3: API lane by final URL shape
       (define-values (kind id) (classify-url final-url))
       (define t-api (now-ms))
       (phase! "Fetching the conversation from grok.com…" 0.14)
       (define data
         (cond
           [(hash-ref opts 'skip-api) (set-run-api-lane! R "skipped") #f]
           [(eq? kind 'share)
            (define-values (chosen chunk legacy) (api-lane-share R c id))
            (set! t-chunk (and chunk (build-transcript chunk final-url)))
            (set! t-legacy (and legacy (build-transcript legacy final-url)))
            chosen]
           [(eq? kind 'conversation) (api-lane-conversation R c id)]
           [else (warn! R "final URL ~a is neither /share/ nor /c/; API lane unavailable" final-url) #f]))
       (timing! R "api" t-api)
       (when (and (not data) (not (hash-ref opts 'skip-api)))
         (set-run-api-lane! R "unavailable")
         (warn! R "API lane unavailable: no endpoint returned a parsable payload with responses; continuing with the DOM lane only"))
       (define conv-id
         (or (and data (let ([v (jget (or-empty-hash (jget data 'conversation)) 'conversationId)]) (and (string? v) v)))
             (and (eq? kind 'conversation) id)
             (and (eq? kind0 'conversation) id0)
             id id0 "unknown"))
       (set-run-conversation-id! R conv-id)
       (make-export-dir! R conv-id)
       ;; step 4 + 7: parse and write the transcript (html is rewritten after attachments/enrichment)
       (when data
         (phase! "Building transcript.json and transcript.md…" 0.22)
         (set! transcript (parse-and-write-transcript! R data final-url #:html #f)))
       ;; SPEC 0.6 item 3: the sandbox previewUrls come from the transcript itself; the deployment
       ;; record has to be asked for (app-deployments takes a conversation id, so only the /c/ lane
       ;; can ask).  A 404 "deployment not found" is recorded like any other answer.
       (cond
         [(hash-ref opts 'skip-api)
          (record-deployment! R (hasheq 'endpoint 'null 'queried #f 'reason "--skip-api"))]
         [(not (eq? kind 'conversation))
          (record-deployment! R (hasheq 'endpoint 'null 'queried #f
                                        'reason (format "~a lane: app-deployments takes a conversation id" kind)))]
         [else (query-app-deployment! R c id)])
       ;; page capture (HTML + full-page screenshot)
       (phase! "Capturing the page and the full-page screenshot…" 0.26)
       (capture-page! R c)
       ;; step 6: attachments (skipped in turbo/transcript-only mode)
       (when (and transcript (not (hash-ref opts 'no-attachments #f)))
         (define-values (ok total) (download-attachments! R c transcript))
         (log-info "attachments: ~a of ~a downloaded" ok total))
       ;; step 5: DOM lane, --rounds times, plus per-turn screenshots after round 1
       (cond
         [(hash-ref opts 'skip-dom) (log-info "DOM lane skipped (--skip-dom)")]
         [else (dom-lane! R c url port)])
       ;; late API retry: grok.com sometimes serves the conversation with its X posts stripped
       ;; (xpostIds present, xposts empty) for tens of seconds after a page load; the quick
       ;; re-fetches in api-fetch may all fall into that window.  The DOM lane took minutes, so
       ;; try the API lane once more now and rebuild the transcript from a hydrated payload.
       (when (and data (not (hash-ref opts 'skip-api)) (xposts-stripped? data))
         (define t-retry (now-ms))
         (let retry ([pass 1])
           (log-info "API payload was served unhydrated (~a); retry pass ~a/~a of the API lane, ~a s after the first pass"
                     (xposts-stripped? data) pass API-LATE-RETRIES (quotient (- (now-ms) t-api) 1000))
           (define suffix (if (= pass 1) "-retry" (format "-retry~a" pass)))
           (define-values (data2 chunk2 legacy2)
             (with-handlers ([exn:fail? (lambda (e) (warn! R "API retry failed: ~a" (exn-message e)) (values #f #f #f))])
               (cond
                 [(eq? kind 'share) (api-lane-share R c id #:suffix suffix)]
                 [(eq? kind 'conversation) (values (api-lane-conversation R c id #:suffix suffix) #f #f)]
                 [else (values #f #f #f)])))
           (cond
             [(and data2 (not (xposts-stripped? data2)))
              (note! R "API retry pass ~a returned a hydrated payload; the transcript is rebuilt from raw/api-*~a.json (the first-pass payload is kept as raw/api-*.json)" pass suffix)
              (when (eq? kind 'share)
                (set! t-chunk (and chunk2 (build-transcript chunk2 final-url)))
                (set! t-legacy (and legacy2 (build-transcript legacy2 final-url))))
              (set! data data2)
              (set! transcript (parse-and-write-transcript! R data2 final-url #:html #f))]
             [(< pass API-LATE-RETRIES)
              (warn! R "API retry pass ~a: payload still unhydrated (~a); next pass in ~a s"
                     pass (if data2 (xposts-stripped? data2) "no payload") API-LATE-RETRY-DELAY-S)
              (sleep API-LATE-RETRY-DELAY-S)
              (retry (add1 pass))]
             [data2 (warn! R "API retry pass ~a: payload still unhydrated (~a); the export keeps the first-pass payload (X posts missing)" pass (xposts-stripped? data2))]
             [else (warn! R "API retry pass ~a produced no payload; the export keeps the first-pass payload" pass)]))
         (timing! R "api-retry" t-retry))
       (when transcript
         (emit-string! R "raw/transcript-api.json" (jsexpr->canonical-string transcript)))
       ;; verification (SPEC 5) on the un-enriched transcript, as the reference verifier does
       (define t-ver (now-ms))
       (phase! "Verifying the export against the page…" 0.93)
       (define verification (assemble-verification R transcript t-chunk t-legacy))
       (set-run-verification! R verification)
       (emit-string! R "verification.json" (jsexpr->canonical-string verification))
       (timing! R "verification" t-ver)
       (define vs (or-empty-hash (jget verification 'summary)))
       (log-info "verification.json: ~a checks, ~a ok, ~a failed, ~a warning(s) -> ok ~a"
                 (jget vs 'total) (jget vs 'ok) (jget vs 'failed)
                 (length (or-empty-list (jget verification 'warnings))) (jget verification 'ok))
       (for ([f (or-empty-list (jget verification 'failedChecks))])
         (log-info "  FAILED ~a turn ~a" (jget f 'name) (jget f 'turnIndex)))
       ;; DOM chip fallback for citations the API left null (SPEC 0.5), then final transcript files
       (when transcript
         (define rounds (run-dom-rounds R))
         (define-values (t* notes)
           (if (pair? rounds)
               (enrich-citations transcript (hash-ref (car rounds) 'capture) (run-chip-links R))
               (values transcript '())))
         (set-run-enrichment! R notes)
         (for ([n notes]) (log-info "citation enrichment: ~a" n))
         (define sources (citation-sources R transcript t*))
         (set-run-citation-sources! R sources)
         (log-info "citation sources: ~a"
                   (string-join (for/list ([(k v) (in-hash (jget sources 'summary))]) (format "~a ~a" k v)) ", "))
         (set! transcript t*)
         (phase! "Writing transcript.html…" 0.97)
         (write-transcript-outputs! R transcript #:html #t))
       (define behavior-ok?
         (if transcript
             (begin
               (phase! "Checking the PDF-derived behavior patterns…" 0.985)
               (write-behavior-reports! R transcript))
             #t))
       (set-run-transport! R (cdp-transport-stats c))
       (cond
         [(and transcript (jget verification 'ok) behavior-ok?)
          (set! exit-code 0)]
         [transcript (set! exit-code 2)]
         [(pair? (run-dom-rounds R)) (set! exit-code 2)]
         [else (set! exit-code 1)])
       (write-manifest! R exit-code
                        #:error (cond [(= exit-code 1) "no API data could be parsed and the DOM lane produced no capture"]
                                      [else #f]))))
   (lambda ()
     (unless (hash-ref opts 'keep-open)
       (with-handlers ([exn:fail? (lambda (e) (warn! R "closing tab ~a failed: ~a" target-id (exn-message e)))])
         (cdp-close c)
         (define-values (code body) (cdp-close-target "127.0.0.1" port target-id))
         (log-info "Closed tab ~a (HTTP ~a ~a)" target-id code (string-trim body))))))
  ;; summary
  (log-info "")
  (log-info "==== ~a ~a summary ====" TOOL-NAME TOOL-VERSION)
  (log-info "export dir : ~a" (if (run-export-dir R) (path->string (run-export-dir R)) "(none)"))
  (log-info "final url  : ~a" (run-final-url R))
  (log-info "api lane   : ~a (parsed format ~a)" (run-api-lane R) (run-parsed-format R))
  (for ([r (in-list (run-raws R))])
    (log-info "  ~a: HTTP ~a, ~a bytes" (hash-ref r 'file) (hash-ref r 'status) (hash-ref r 'bytes)))
  (log-info "counts     : ~a" (jsexpr->string (run-counts R)))
  (log-info "dom lane   : ~a round(s), ~a screenshot(s)" (length (run-dom-rounds R)) (length (run-screenshots R)))
  (for ([d (in-list (run-dom-rounds R))])
    (define s (hash-ref d 'stats))
    (log-info "  round ~a: ~a articles, ~a thoughts / ~a sources panels, remainingCollapsed ~a, extractor ~a ms"
              (hash-ref d 'round) (jget s 'articles) (jget s 'thoughtsPanels) (jget s 'sourcesPanels)
              (jget s 'remainingCollapsedTotal) (hash-ref d 'extractorMs)))
  (log-info "websocket  : ~a" (jsexpr->string (run-transport R)))
  (log-info "files      : ~a" (length (run-files R)))
  (for ([f (in-list (run-files R))])
    (log-info "  ~a  ~a bytes  sha256 ~a" (hash-ref f 'path) (hash-ref f 'sizeBytes) (hash-ref f 'sha256)))
  (log-info "warnings   : ~a" (length (run-warnings R)))
  (for ([w (in-list (run-warnings R))]) (log-info "  - ~a" w))
  (define v (run-verification R))
  (cond
    [(hash? v)
     (define s (or-empty-hash (jget v 'summary)))
     (log-info "verification: ~a checks, ~a ok, ~a failed; api ~a; dom ~a; ok ~a"
               (jget s 'total) (jget s 'ok) (jget s 'failed) (jget v 'api) (jget v 'dom) (jget v 'ok))
     (for ([w (or-empty-list (jget v 'warnings))]) (log-info "  ! ~a" w))]
    [else (log-info "verification: not run")])
  (define behavior (run-behavior R))
  (cond
    [(hash? behavior)
     (define s (or-empty-hash (jget behavior 'summary)))
     (log-info "behavior    : ~a finding(s), ~a confirmed, ~a candidate, ~a not assessable; status ~a"
               (length (or-empty-list (jget behavior 'findings)))
               (jget s 'confirmed) (jget s 'candidate) (jget s 'notAssessable)
               (run-behavior-status R))]
    [else (log-info "behavior    : ~a~a"
                    (run-behavior-status R)
                    (if (run-behavior-error R)
                        (format " (~a)" (run-behavior-error R))
                        ""))])
  (phase! "Finished." 1.0)
  (log-info "exit code  : ~a" exit-code)
  (when (run-export-dir R) (log-info "EXPORT_DIR=~a" (path->string (run-export-dir R))))
  exit-code)

;; ------------------------------------------------------------------ main
(define (main argv)
  (define opts
    (with-handlers ([exn:fail? (lambda (e) (log-info "~a" (exn-message e)) (usage) #f)])
      (parse-args argv)))
  (cond
    [(not opts) 1]
    [(hash-ref opts 'help) (usage) 0]
    [else
     (parameterize ([verbose? (hash-ref opts 'verbose)])
       (define R (make-run opts))
       (with-handlers ([exn:fail? (lambda (e) (log-info "ERROR: ~a" (exn-message e)) 1)])
         (check-embedded-js!)
         (cond
           [(hash-ref opts 'from-json) (run-from-json R)]
           [(hash-ref opts 'verify-transcript) (run-verify-only R)]
           [(not (hash-ref opts 'url)) (usage) 1]
           [else (run-live R)])))]))

(module+ main
  (exit (main (current-command-line-arguments))))
