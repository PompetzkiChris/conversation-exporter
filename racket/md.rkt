#lang racket/base
;; md.rkt — transcript.md writer, a port of reference_transcript.py `to_markdown`.
(require racket/string
         json
         "transcript.rkt"
         "jsonw.rkt")

;; pretty-json-string is shared with html.rkt so the two renderers show an mcp card's
;; toolArgsJson the same way (SPEC 0.6 item 2).
(provide transcript->markdown
         pretty-json-string)

(define TOOL-LABELS
  (hash "webSearch" "Searched web"
        "xSearch" "Searched X"
        "xUserSearch" "Searched X users"
        "browsePage" "Browsed"
        "conversationSearch" "Searching conversations"
        "viewImage" "View Image"
        "initTerminalSession" "Connected to computer"
        "chatroomSend" "Sent to All"
        ;; SPEC 0.6 (Build mode) — none of these kinds occurs in a Heavy conversation,
        ;; so the Heavy markdown is unchanged by their presence here.
        "bash" "Ran command"
        "editFile" "Wrote file"
        "readFile" "Read file"
        "listDir" "Listed directory"
        "imageSearch" "Searched images"
        "mcp" "Tool"))

;; kinds that get the SPEC 0.6 rendering (command / file content / path / description)
(define BUILD-KINDS '("bash" "editFile" "readFile" "listDir" "imageSearch" "mcp"))

;; one_line(s, limit): (s or "") with \r removed and \n -> space, cut to `limit` code points.
(define (one-line s [limit #f])
  (define s0 (cond [(string? s) s]
                   [(truthy? s) (py-str s)]
                   [else ""]))
  (define s1 (string-replace (string-replace s0 "\r" "") "\n" " "))
  (if (and limit (> (string-length s1) limit)) (substring s1 0 limit) s1))

(define (text-with-citation-marks text cits)
  (cond
    [(null? cits) text]
    [else
     (define out (open-output-string))
     (define n (string-length text))
     (let loop ([cs cits] [i 1] [last 0])
       (cond
         [(null? cs) (write-string text out last n)]
         [else
          (define off0 (jget (car cs) 'offset))
          (define off (min (max (if (exact-integer? off0) off0 0) 0) n))
          (when (> off last) (write-string text out last off))
          (write-string (format "[~a]" i) out)
          (loop (cdr cs) (add1 i) off)]))
     (get-output-string out)]))

(define (int-or v default)
  (if (exact-integer? v) v default))

;; ------------------------------------------------------------------ SPEC 0.6 helpers
;; args lookup that accepts the camelCase spelling of the structured tool card and the
;; snake_case spelling of the <xai:tool_args> CDATA fallback; "" when absent.
(define (arg-str a . keys)
  (let loop ([ks keys])
    (cond
      [(null? ks) ""]
      [else
       (define v (jget a (car ks)))
       (cond [(string? v) v]
             [(eq? v 'null) (loop (cdr ks))]
             [else (py-str v)])])))

(define (non-empty? s) (and (string? s) (> (string-length s) 0)))

;; longest run of backticks in s (so a fence can always be made longer than it)
(define (max-backtick-run s)
  (let loop ([i 0] [run 0] [best 0])
    (cond
      [(>= i (string-length s)) (max run best)]
      [(char=? (string-ref s i) #\`) (loop (add1 i) (add1 run) best)]
      [else (loop (add1 i) 0 (max run best))])))

(define EXT-LANG
  (hash "py" "python" "sh" "bash" "bash" "bash" "js" "javascript" "mjs" "javascript"
        "cjs" "javascript" "ts" "typescript" "tsx" "tsx" "jsx" "jsx" "json" "json"
        "md" "markdown" "html" "html" "htm" "html" "css" "css" "c" "c" "h" "c"
        "cpp" "cpp" "hpp" "cpp" "rkt" "racket" "rs" "rust" "go" "go" "java" "java"
        "rb" "ruby" "php" "php" "sql" "sql" "yml" "yaml" "yaml" "yaml" "toml" "toml"
        "xml" "xml" "svg" "xml" "ini" "ini" "csv" "csv" "txt" ""))

(define (path-language p)
  (define m (regexp-match #px"\\.([A-Za-z0-9_]+)$" (or p "")))
  (if m (hash-ref EXT-LANG (string-downcase (cadr m)) "") ""))

;; A fenced block indented two spaces so it stays inside the bullet, preceded and followed
;; by a blank line; the fence is longer than any backtick run in the body.
(define (emit-fenced! L lang body)
  (define b (string-replace (or body "") "\r" ""))
  (define fence (make-string (max 3 (add1 (max-backtick-run b))) #\`))
  (L "")
  (L (string-append "  " fence lang))
  (for ([line (in-list (regexp-split #rx"\n" b))])
    (L (if (string=? line "") "" (string-append "  " line))))
  (L (string-append "  " fence))
  (L ""))

;; Display-only JSON pretty printer.  NOT the canonical writer: an mcp tool's toolArgsJson
;; may carry floats (e.g. {"strength":0.6}) and jsonw's canonical writer rejects every
;; non-integer number by design, which would abort the whole markdown file.
(define (write-pretty-json v out ind)
  (define pad (make-string ind #\space))
  (define pad+ (make-string (add1 ind) #\space))
  (cond
    [(hash? v)
     (define ks (sort (for/list ([k (in-hash-keys v)]) (if (symbol? k) (symbol->string k) (format "~a" k)))
                      string<?))
     (cond
       [(null? ks) (write-string "{}" out)]
       [else
        (write-string "{\n" out)
        (for ([k (in-list ks)] [i (in-naturals)])
          (unless (zero? i) (write-string ",\n" out))
          (write-string pad+ out)
          (write-json-string-literal k out)
          (write-string ": " out)
          (write-pretty-json (hash-ref v (string->symbol k) 'null) out (add1 ind)))
        (write-string "\n" out)
        (write-string pad out)
        (write-string "}" out)])]
    [(and (list? v) (null? v)) (write-string "[]" out)]
    [(list? v)
     (write-string "[\n" out)
     (for ([x (in-list v)] [i (in-naturals)])
       (unless (zero? i) (write-string ",\n" out))
       (write-string pad+ out)
       (write-pretty-json x out (add1 ind)))
     (write-string "\n" out)
     (write-string pad out)
     (write-string "]" out)]
    [(string? v) (write-json-string-literal v out)]
    [(eq? v #t) (write-string "true" out)]
    [(eq? v #f) (write-string "false" out)]
    [(eq? v 'null) (write-string "null" out)]
    [(exact-integer? v) (write-string (number->string v) out)]
    [(and (real? v) (rational? v)) (write-string (number->string (exact->inexact v)) out)]
    [else (write-json-string-literal (format "~a" v) out)]))

;; toolArgsJson is a JSON *string*; show it parsed (sorted keys) when it parses, raw otherwise.
(define (pretty-json-string s)
  (with-handlers ([exn:fail? (lambda (e) s)])
    (define v (string->jsexpr s))
    (cond
      [(or (hash? v) (list? v))
       (define out (open-output-string))
       (write-pretty-json v out 0)
       (get-output-string out)]
      [else s])))

(define (pretty-json-value v)
  (define out (open-output-string))
  (write-pretty-json v out 0)
  (get-output-string out))

;; generatedImageUrls, imageEditUris and imageAttachments are response-level image surfaces.
;; The transcript keeps their raw arrays; Markdown makes every item visible without flattening
;; structured imageAttachments into an ambiguous string.
(define (emit-image-surfaces! L turn)
  (define any? #f)
  (for ([entry (in-list (list (cons "Generated image" 'generatedImageUrls)
                              (cons "Image edit URI" 'imageEditUris)
                              (cons "Image attachment" 'imageAttachments)))])
    (for ([v (in-list (or-empty-list (jget turn (cdr entry))))])
      (set! any? #t)
      (cond
        [(string? v) (L (format "- **~a:** ~a" (car entry) v))]
        [else
         (L (format "- **~a:**" (car entry)))
         (emit-fenced! L "json" (pretty-json-value v))])))
  (when any? (L "")))

(define (count-suffix items)
  (if (pair? items) (format "  (~a results)" (length items)) ""))

;; result items of a tool event, one bullet each (unchanged Heavy rendering)
(define (emit-results! L res)
  (define items (or-empty-list (jget res 'items)))
  (define kind (jget res 'kind))
  (for ([it (in-list items)])
    (cond
      [(equal? kind "web")
       (L (format "  - [~a](~a)"
                  (one-line (py-or (jget it 'title) (jget it 'url) ""))
                  (py-str (jget it 'url))))]
      [(equal? kind "x")
       (L (format "  - @~a (~a): ~a"
                  (py-str (jget it 'username)) (py-str (jget it 'postId))
                  (one-line (jget it 'text) 200)))]
      [else (void)])))

;; SPEC 0.6 item 2: bash / editFile / readFile / listDir / imageSearch / mcp.
(define (emit-build-tool! L label k a res)
  (define items (or-empty-list (jget res 'items)))
  (define suffix (count-suffix items))
  (define (head s) (L (format "- **~a** ~a~a" (py-str label) s suffix)))
  (cond
    [(equal? k "bash")
     (head (one-line (arg-str a 'description)))
     (define cmd (arg-str a 'command))
     (when (non-empty? cmd) (emit-fenced! L "bash" cmd))]
    [(equal? k "editFile")
     (define fp (arg-str a 'filePath 'file_path))
     (define olds (arg-str a 'oldString 'old_string))
     (define news (arg-str a 'newString 'new_string))
     (define lang (path-language fp))
     (head fp)
     (when (non-empty? olds)
       (L "")
       (L "  Replaced:")
       (emit-fenced! L lang olds)
       (L "  With:"))
     (when (or (non-empty? news) (non-empty? olds))
       (emit-fenced! L lang news))]
    [(equal? k "readFile")
     (define details
       (filter values
               (list (let ([v (arg-str a 'fileType 'file_type)]) (and (non-empty? v) v))
                     (let ([v (jget a 'offset)]) (and (exact-integer? v) (format "offset ~a" v)))
                     (let ([v (jget a 'limit)]) (and (exact-integer? v) (format "limit ~a" v))))))
     (head (string-append (arg-str a 'filePath 'file_path)
                          (if (null? details) "" (format "  (~a)" (string-join details "; ")))))]
    [(equal? k "listDir")
     (head (arg-str a 'targetDirectory 'target_directory))]
    [(equal? k "imageSearch")
     (head (one-line (arg-str a 'imageDescription 'image_description)))
     (emit-results! L res)]
    [else                                   ; mcp
     (head (arg-str a 'toolName 'tool_name))
     (define aj (arg-str a 'toolArgsJson 'tool_args_json))
     (when (non-empty? aj) (emit-fenced! L "json" (pretty-json-string aj)))
     (emit-results! L res)]))

(define (transcript->markdown t)
  (define out (open-output-string))
  (define (L s) (write-string s out) (write-char #\newline out))
  (define c (jget t 'conversation))
  (L (string-append "# " (py-str (py-or (jget c 'title) (jget c 'conversationId) ""))))
  (L "")
  (L (string-append "- Conversation: " (py-str (jget c 'conversationId))))
  (L (string-append "- Source: " (py-str (jget c 'sourceUrl))))
  (L (string-append "- Created: " (py-str (jget c 'createTime))))
  (L (string-append "- Modified: " (py-str (jget c 'modifyTime))))
  (L "")
  (for ([turn (in-list (or-empty-list (jget t 'turns)))])
    (cond
      [(equal? (jget turn 'sender) "human")
       (L (format "## User  (turn ~a, ~a)" (py-str (jget turn 'index)) (py-str (jget turn 'createTime))))
       (L "")
       (emit-image-surfaces! L turn)
       (define atts (or-empty-list (jget turn 'attachments)))
       (for ([a (in-list atts)])
         (L (format "- Attachment: ~a (~a, ~a bytes) ~a"
                    (py-str (jget a 'fileName)) (py-str (jget a 'mimeType))
                    (py-str (jget a 'sizeBytes)) (py-str (jget a 'contentUrl)))))
       (when (pair? atts) (L ""))
       (L (py-str (jget turn 'text)))
       (L "")]
      [else
       (define th (or-empty-hash (jget turn 'thinking)))
       (L (format "## Grok  (turn ~a, ~a, model ~a)"
                  (py-str (jget turn 'index)) (py-str (jget turn 'createTime)) (py-str (jget turn 'model))))
       (L "")
       (emit-image-surfaces! L turn)
       (L (format "### Thoughts  (~a ms)" (py-str (jget th 'durationMs))))
       (L "")
       (for ([rl (in-list (or-empty-list (jget th 'rollouts)))])
         (L (format "#### ~a~a" (py-str (jget rl 'id))
                    (if (equal? (jget rl 'role) "Leader") " (Leader)" "")))
         (L "")
         (for ([ev (in-list (or-empty-list (jget rl 'events)))])
           (define ty (jget ev 'type))
           (cond
             [(equal? ty "summary")
              (L (format "- _~a_" (one-line (jget ev 'text))))]
             [(equal? ty "tool")
              (define k (jget ev 'kind))
              (define a (or-empty-hash (jget ev 'args)))
              (define label (if (and (string? k) (hash-has-key? TOOL-LABELS k)) (hash-ref TOOL-LABELS k) k))
              (cond
                [(equal? k "chatroomSend")
                 (L "- **Sent to All:**")
                 (L "")
                 (define msg (let ([m (py-or (jget a 'message) "")]) (if (string? m) m (py-str m))))
                 ;; regexp-split == Python str.split("\n"): "" -> (""), "a\n" -> ("a" "")
                 (for ([line (in-list (regexp-split #rx"\n" (string-replace msg "\r" "")))])
                   (L (string-append "  " line)))
                 (L "")]
                [(and (string? k) (member k BUILD-KINDS))
                 (emit-build-tool! L label k a (or-empty-hash (jget ev 'results)))]
                [else
                 (define qv (jget a 'query))
                 ;; previewUrl is the whole payload of a Build-mode initTerminalSession card
                 ;; (SPEC 0.6); it is null in every Heavy card, so Heavy markdown is unchanged.
                 (define q (if (not (eq? qv 'null))
                               qv
                               (py-or (jget a 'url) (jget a 'previewUrl) (jget a 'preview_url) "")))
                 (define res (or-empty-hash (jget ev 'results)))
                 (define items (or-empty-list (jget res 'items)))
                 (L (format "- **~a** ~a~a" (py-str label) (py-str q) (count-suffix items)))
                 (emit-results! L res)])]
             [(equal? ty "tool_result")
              (define res (or-empty-hash (jget ev 'results)))
              (L (format "- tool result ~a: ~a items" (py-str (jget ev 'toolCallId))
                         (length (or-empty-list (jget res 'items)))))]
             [(equal? ty "text")
              (L (format "- [~a] ~a" (py-str (jget ev 'channel)) (one-line (jget ev 'text) 200)))]
             [else (L (format "- ~a" (py-str ty)))]))
         (L ""))
       (L "### Reply")
       (L "")
       (define cits (or-empty-list (jget turn 'citations)))
       (L (text-with-citation-marks (py-str (jget turn 'text)) cits))
       (L "")
       (when (pair? cits)
         (L "Citations:")
         (L "")
         (for ([ci (in-list cits)] [n (in-naturals 1)])
           (define u (jget ci 'url))
           (L (format "- [~a] ~a (citationId ~a, card ~a)" n
                      (if (truthy? u) (py-str u) "unresolved")
                      (py-str (jget ci 'citationId)) (py-str (jget ci 'cardId)))))
         (L ""))
       (define s (or-empty-hash (jget turn 'sources)))
       (define webs (or-empty-list (jget s 'webSearchResults)))
       (define xs (or-empty-list (jget s 'xposts)))
       (L (format "### Sources  (~a web, ~a X posts, ~a tool result rows)"
                  (length webs) (length xs) (int-or (jget s 'toolResultRows) 0)))
       (L "")
       (for ([w (in-list webs)])
         (L (format "- [~a](~a)" (one-line (py-or (jget w 'title) (jget w 'url) "")) (py-str (jget w 'url)))))
       (for ([p (in-list xs)])
         (L (format "- X @~a ~a: ~a" (py-str (jget p 'username)) (py-str (jget p 'postId))
                    (one-line (jget p 'text) 200))))
       (L "")]))
  (get-output-string out))
