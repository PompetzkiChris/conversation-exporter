#lang racket/base
;; html.rkt — transcript.html writer: self-contained, dark theme, everything expanded,
;; one <details open> per rollout, clickable links, attachments embedded as <img> from the
;; attachments/ folder (relative path).
(require racket/string
         racket/list
         json
         "transcript.rkt"
         (only-in "md.rkt" pretty-json-string))

(provide transcript->html)

(define TOOL-LABELS
  (hash "webSearch" "Searched web"
        "xSearch" "Searched X"
        "xUserSearch" "Searched X users"
        "browsePage" "Browsed"
        "conversationSearch" "Searching conversations"
        "viewImage" "View Image"
        "initTerminalSession" "Connected to computer"
        "chatroomSend" "Sent to All"
        ;; SPEC 0.6 (Build mode) — the same labels md.rkt uses.  None of these kinds occurs in a
        ;; Heavy conversation, so a Heavy page renders exactly as it did before they were added.
        "bash" "Ran command"
        "editFile" "Wrote file"
        "readFile" "Read file"
        "listDir" "Listed directory"
        "imageSearch" "Searched images"
        "mcp" "Tool"))

;; kinds that get the SPEC 0.6 rendering (command / file content / path / description)
(define BUILD-KINDS '("bash" "editFile" "readFile" "listDir" "imageSearch" "mcp"))

(define (esc v)
  (define s (cond [(string? v) v] [(eq? v 'null) ""] [else (py-str v)]))
  (define out (open-output-string))
  (for ([c (in-string s)])
    (case c
      [(#\&) (write-string "&amp;" out)]
      [(#\<) (write-string "&lt;" out)]
      [(#\>) (write-string "&gt;" out)]
      [(#\") (write-string "&quot;" out)]
      [else (write-char c out)]))
  (get-output-string out))

(define (url? v) (and (string? v) (regexp-match? #px"^https?://" v)))

(define (link u [text #f])
  (if (url? u)
      (format "<a href=\"~a\" target=\"_blank\" rel=\"noopener noreferrer\">~a</a>" (esc u) (esc (or text u)))
      (esc (or text u))))

;; Reply text with [n] citation marks (1-based) inserted at the citation offsets, escaped.
(define (reply-html text cits turn-index)
  (define out (open-output-string))
  (define n (string-length text))
  (let loop ([cs cits] [k 1] [last 0])
    (cond
      [(null? cs) (write-string (esc (substring text last n)) out)]
      [else
       (define off0 (jget (car cs) 'offset))
       (define off (min (max (if (exact-integer? off0) off0 0) 0) n))
       (define off* (max off last))
       (write-string (esc (substring text last off*)) out)
       (write-string (format "<sup class=\"cite\"><a href=\"#cite-~a-~a\">[~a]</a></sup>" turn-index k k) out)
       (loop (cdr cs) (add1 k) off*)]))
  (get-output-string out))

(define CSS
  (string-append
   "body{background:#111418;color:#d7dce2;font:15px/1.5 -apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;margin:0;padding:24px;}"
   "main{max-width:1100px;margin:0 auto;}"
   "h1{font-size:22px;color:#fff;margin:0 0 6px;}"
   ".meta{color:#8b95a1;font-size:13px;margin-bottom:24px;} .meta a{color:#7fb3ff;}"
   "a{color:#7fb3ff;text-decoration:none;} a:hover{text-decoration:underline;}"
   ".turn{border:1px solid #262c34;border-radius:10px;padding:16px 18px;margin:0 0 18px;background:#161a1f;}"
   ".turn.user{background:#1b2230;border-color:#2b3a52;}"
   ".turn-head{font-weight:600;color:#fff;margin-bottom:8px;} .turn-head .ts{color:#8b95a1;font-weight:400;font-size:12px;margin-left:8px;}"
   ".text{white-space:pre-wrap;word-wrap:break-word;}"
   ".attachments{margin:6px 0 12px;} .attachments figure{display:inline-block;margin:0 12px 8px 0;vertical-align:top;}"
   ".attachments img{max-width:420px;max-height:420px;border:1px solid #2a323c;border-radius:6px;display:block;}"
   ".attachments figcaption{font-size:12px;color:#8b95a1;margin-top:4px;}"
   "details{border:1px solid #2a323c;border-radius:8px;margin:8px 0;background:#12161b;}"
   "summary{cursor:pointer;padding:8px 12px;font-weight:600;color:#e6ebf0;} summary .role{color:#f0b35a;font-weight:400;font-size:12px;margin-left:6px;}"
   ".events{padding:4px 14px 10px 14px;} .ev{margin:6px 0;padding-left:10px;border-left:2px solid #2a323c;}"
   ".ev.summary{color:#aeb7c2;font-style:italic;} .ev.tool .label{color:#9ad0a0;font-weight:600;} .ev.tool .q{color:#e6ebf0;}"
   ".ev.chatroom .label{color:#f0b35a;font-weight:600;} .ev.chatroom .msg{white-space:pre-wrap;background:#0e1114;border-radius:6px;padding:8px 10px;margin-top:4px;}"
   ;; SPEC 0.6 payload blocks (bash command, written file, mcp arguments).  Only Build-mode
   ;; cards use these classes, so a Heavy page is unaffected.
   ".ev.tool pre.code{white-space:pre-wrap;word-wrap:break-word;overflow-x:auto;background:#0e1114;border:1px solid #232a32;border-radius:6px;padding:8px 10px;margin:4px 0 0;font:12px/1.45 Consolas,Menlo,Monaco,monospace;color:#cdd6df;}"
   ".ev.tool .sub{color:#8b95a1;font-size:12px;margin-top:6px;}"
   ".results{margin:4px 0 0 0;padding-left:18px;font-size:13px;color:#aeb7c2;} .results li{margin:2px 0;}"
   ".results .prev{color:#7d8794;} .xpost{color:#aeb7c2;}"
   ".section-title{margin:16px 0 6px;font-size:13px;letter-spacing:.06em;text-transform:uppercase;color:#8b95a1;}"
   "sup.cite a{color:#f0b35a;font-size:11px;} ol.cites{font-size:13px;color:#aeb7c2;}"
   ".sources ul{padding-left:18px;font-size:13px;color:#aeb7c2;} .sources li{margin:2px 0;}"
   ".warn{color:#f0b35a;}"))

(define (one-line s [limit #f])
  (define s0 (cond [(string? s) s] [(truthy? s) (py-str s)] [else ""]))
  (define s1 (string-replace (string-replace s0 "\r" "") "\n" " "))
  (if (and limit (> (string-length s1) limit)) (substring s1 0 limit) s1))

;; result items of a tool event as the <ul class="results"> block ("" when there are none).
;; Byte-for-byte what the Heavy renderer emitted before the SPEC 0.6 branch was added.
(define (results-html res)
  (define items (or-empty-list (jget res 'items)))
  (define kind (jget res 'kind))
  (if (pair? items)
      (string-append
       "<ul class=\"results\">\n"
       (apply string-append
              (for/list ([it items])
                (cond
                  [(equal? kind "web")
                   (format "<li>~a~a</li>\n"
                           (link (jget it 'url) (one-line (py-or (jget it 'title) (jget it 'url) "")))
                           (let ([p (jget it 'preview)])
                             (if (truthy? p) (format " <span class=\"prev\">— ~a</span>" (esc (one-line p 300))) "")))]
                  [(equal? kind "x")
                   (define u (format "https://x.com/~a/status/~a" (py-str (jget it 'username)) (py-str (jget it 'postId))))
                   (format "<li class=\"xpost\">~a: ~a</li>\n"
                           (link u (format "@~a (~a)" (py-str (jget it 'username)) (py-str (jget it 'postId))))
                           (esc (one-line (jget it 'text) 300)))]
                  [else (format "<li>~a</li>\n" (esc (py-str it)))])))
       "</ul>\n")
      ""))

;; ------------------------------------------------------------------ SPEC 0.6 tool cards
;; args lookup that accepts the camelCase spelling of the structured tool card and the
;; snake_case spelling of the <xai:tool_args> CDATA fallback; "" when absent (as in md.rkt).
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

(define (pre-block s) (format "<pre class=\"code\">~a</pre>\n" (esc (string-replace (or s "") "\r" ""))))

(define (sub-line s) (format "<div class=\"sub\">~a</div>\n" (esc s)))

;; "<div class="ev tool"><span class="label">L</span> <span class="q">Q</span> (n results)\n"
(define (tool-head label q items)
  (format "<div class=\"ev tool\"><span class=\"label\">~a</span> <span class=\"q\">~a</span>~a\n"
          (esc label)
          (if (url? q) (link q) (esc q))
          (if (pair? items) (format " <span class=\"prev\">(~a results)</span>" (length items)) "")))

;; SPEC 0.6 item 2, the same content md.rkt renders: bash -> description + the command,
;; editFile -> path + the written file (and the replaced text when there is one),
;; readFile/listDir -> the path, imageSearch -> the description, mcp -> tool name + parsed
;; toolArgsJson.  Every payload goes through `esc`, so it is inert inside the page.
(define (build-tool-html label k a res)
  (define items (or-empty-list (jget res 'items)))
  (define head (lambda (q) (tool-head label q items)))
  (string-append
   (cond
     [(equal? k "bash")
      (define cmd (arg-str a 'command))
      (string-append (head (one-line (arg-str a 'description)))
                     (if (non-empty? cmd) (pre-block cmd) ""))]
     [(equal? k "editFile")
      (define fp (arg-str a 'filePath 'file_path))
      (define olds (arg-str a 'oldString 'old_string))
      (define news (arg-str a 'newString 'new_string))
      (string-append (head fp)
                     (if (non-empty? olds)
                         (string-append (sub-line "Replaced:") (pre-block olds) (sub-line "With:"))
                         "")
                     (if (or (non-empty? news) (non-empty? olds)) (pre-block news) ""))]
      [(equal? k "readFile")
       (define details
         (filter values
                 (list (let ([v (arg-str a 'fileType 'file_type)]) (and (non-empty? v) v))
                       (let ([v (jget a 'offset)]) (and (exact-integer? v) (format "offset ~a" v)))
                       (let ([v (jget a 'limit)]) (and (exact-integer? v) (format "limit ~a" v))))))
       (string-append (head (arg-str a 'filePath 'file_path))
                      (if (null? details) "" (sub-line (string-join details "; "))))]
     [(equal? k "listDir") (head (arg-str a 'targetDirectory 'target_directory))]
     [(equal? k "imageSearch")
      (string-append (head (one-line (arg-str a 'imageDescription 'image_description)))
                     (results-html res))]
     [else                                   ; mcp
      (define aj (arg-str a 'toolArgsJson 'tool_args_json))
      (string-append (head (arg-str a 'toolName 'tool_name))
                     (if (non-empty? aj) (pre-block (pretty-json-string aj)) "")
                     (results-html res))])
   "</div>\n"))

(define (pretty-json-value v)
  (pretty-json-string (jsexpr->string v)))

(define (image-surfaces-html turn)
  (define rows
    (apply
     append
     (for/list ([entry (in-list (list (cons "Generated image" 'generatedImageUrls)
                                      (cons "Image edit URI" 'imageEditUris)
                                      (cons "Image attachment" 'imageAttachments)))])
       (for/list ([v (in-list (or-empty-list (jget turn (cdr entry))))])
         (format "<div class=\"image-surface\"><span class=\"label\">~a</span> ~a</div>\n"
                 (esc (car entry))
                 (cond [(and (string? v) (url? v)) (link v)]
                       [(string? v) (format "<code>~a</code>" (esc v))]
                       [else (pre-block (pretty-json-value v))]))))))
  (if (null? rows) "" (string-append "<div class=\"image-surfaces\">\n" (apply string-append rows) "</div>\n")))

(define (event-html ev)
  (define ty (jget ev 'type))
  (cond
    [(equal? ty "summary")
     (format "<div class=\"ev summary\">~a</div>\n" (esc (jget ev 'text)))]
    [(equal? ty "tool")
     (define k (jget ev 'kind))
     (define a (or-empty-hash (jget ev 'args)))
     (define label (if (and (string? k) (hash-has-key? TOOL-LABELS k)) (hash-ref TOOL-LABELS k) (py-str k)))
     (cond
       [(equal? k "chatroomSend")
        (define msg (let ([m (py-or (jget a 'message) "")]) (if (string? m) m (py-str m))))
        (format "<div class=\"ev chatroom\"><span class=\"label\">Sent to All</span><div class=\"msg\">~a</div></div>\n" (esc msg))]
       [(and (string? k) (member k BUILD-KINDS))
        (build-tool-html label k a (or-empty-hash (jget ev 'results)))]
       [else
        (define qv (jget a 'query))
        ;; previewUrl is the whole payload of a Build-mode initTerminalSession card; it is null
        ;; in every Heavy card, so the Heavy page is unchanged.
        (define q (if (not (eq? qv 'null)) qv (py-or (jget a 'url) (jget a 'previewUrl) (jget a 'preview_url) "")))
        (define res (or-empty-hash (jget ev 'results)))
        (define items (or-empty-list (jget res 'items)))
        (define instr (jget a 'instructions))
        (string-append
         (format "<div class=\"ev tool\"><span class=\"label\">~a</span> <span class=\"q\">~a</span>~a~a\n"
                 (esc label)
                 (if (url? q) (link q) (esc q))
                 (if (pair? items) (format " <span class=\"prev\">(~a results)</span>" (length items)) "")
                 (if (string? instr) (format "<div class=\"prev\">~a</div>" (esc instr)) ""))
         (results-html res)
         "</div>\n")])]
    [(equal? ty "tool_result")
     (define res (or-empty-hash (jget ev 'results)))
     (format "<div class=\"ev\">tool result ~a: ~a items</div>\n" (esc (jget ev 'toolCallId)) (length (or-empty-list (jget res 'items))))]
    [(equal? ty "text")
     (format "<div class=\"ev\">[~a] ~a</div>\n" (esc (jget ev 'channel)) (esc (one-line (jget ev 'text) 400)))]
    [else (format "<div class=\"ev\">~a</div>\n" (esc ty))]))

;; attachment-paths: hash fileId -> relative path (e.g. "attachments/0-<id>-image.png"), or #f entries
(define (transcript->html t #:attachment-paths [att-paths (hash)] #:generator [gen "grok-export-rkt"])
  (define out (open-output-string))
  (define (W . xs) (for ([x xs]) (write-string x out)))
  (define c (or-empty-hash (jget t 'conversation)))
  (define title (py-str (py-or (jget c 'title) (jget c 'conversationId) "")))
  (W "<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n"
     "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
     "<meta name=\"generator\" content=\"" (esc gen) "\">\n"
     "<title>" (esc title) "</title>\n<style>" CSS "</style>\n</head>\n<body>\n<main>\n")
  (W "<h1>" (esc title) "</h1>\n")
  (W "<div class=\"meta\">Conversation " (esc (jget c 'conversationId))
     " · Source " (link (jget c 'sourceUrl))
     " · Created " (esc (jget c 'createTime))
     " · Modified " (esc (jget c 'modifyTime))
     (let ([p (jget c 'isPublic)]) (if (eq? p 'null) "" (format " · Public ~a" (py-str p))))
     "</div>\n")
  (for ([turn (in-list (or-empty-list (jget t 'turns)))])
    (define idx (jget turn 'index))
    (cond
      [(equal? (jget turn 'sender) "human")
       (W (format "<section class=\"turn user\" id=\"turn-~a\">\n" idx))
       (W (format "<div class=\"turn-head\">User <span class=\"ts\">turn ~a · ~a</span></div>\n" idx (esc (jget turn 'createTime))))
       (W (image-surfaces-html turn))
       (define atts (or-empty-list (jget turn 'attachments)))
       (when (pair? atts)
         (W "<div class=\"attachments\">\n")
         (for ([a atts])
           (define fid (jget a 'fileId))
           (define rel (hash-ref att-paths fid #f))
           (define mime (py-str (jget a 'mimeType)))
           (W "<figure>")
           (cond
             [(and rel (regexp-match? #px"^image/" mime))
              (W (format "<a href=\"~a\"><img src=\"~a\" alt=\"~a\"></a>" (esc rel) (esc rel) (esc (jget a 'fileName))))]
             [rel (W (format "<a href=\"~a\">~a</a>" (esc rel) (esc (jget a 'fileName))))]
             [else (W (link (jget a 'contentUrl) (py-str (jget a 'fileName))))])
           (W (format "<figcaption>~a (~a, ~a bytes)~a</figcaption>"
                      (esc (jget a 'fileName)) (esc mime) (esc (jget a 'sizeBytes))
                      (if rel "" " <span class=\"warn\">not downloaded</span>")))
           (W "</figure>\n"))
         (W "</div>\n"))
       (W "<div class=\"text\">" (esc (jget turn 'text)) "</div>\n</section>\n")]
      [else
       (define th (or-empty-hash (jget turn 'thinking)))
       (W (format "<section class=\"turn assistant\" id=\"turn-~a\">\n" idx))
       (W (format "<div class=\"turn-head\">Grok <span class=\"ts\">turn ~a · ~a · model ~a</span></div>\n"
                  idx (esc (jget turn 'createTime)) (esc (jget turn 'model))))
       (W (image-surfaces-html turn))
       (W (format "<div class=\"section-title\">Thoughts (~a ms)</div>\n" (esc (jget th 'durationMs))))
       (for ([rl (in-list (or-empty-list (jget th 'rollouts)))])
         (W "<details open><summary>" (esc (jget rl 'id))
            (if (equal? (jget rl 'role) "Leader") "<span class=\"role\">Leader</span>" "")
            (format "<span class=\"role\">~a events</span>" (length (or-empty-list (jget rl 'events))))
            "</summary>\n<div class=\"events\">\n")
         (for ([ev (in-list (or-empty-list (jget rl 'events)))]) (W (event-html ev)))
         (W "</div>\n</details>\n"))
       (W "<div class=\"section-title\">Reply</div>\n")
       (define cits (or-empty-list (jget turn 'citations)))
       (W "<div class=\"text\">" (reply-html (py-str (jget turn 'text)) cits idx) "</div>\n")
       (when (pair? cits)
         (W "<ol class=\"cites\">\n")
         (for ([ci cits] [n (in-naturals 1)])
           (define u (jget ci 'url))
           (W (format "<li id=\"cite-~a-~a\">~a <span class=\"prev\">(citationId ~a, card ~a~a)</span></li>\n"
                      idx n
                      (if (truthy? u) (link u) "unresolved")
                      (esc (jget ci 'citationId)) (esc (jget ci 'cardId))
                      (let ([k (jget ci 'kind)]) (if (truthy? k) (format ", ~a" (esc k)) "")))))
         (W "</ol>\n"))
       (define s (or-empty-hash (jget turn 'sources)))
       (define webs (or-empty-list (jget s 'webSearchResults)))
       (define xs (or-empty-list (jget s 'xposts)))
       (W (format "<div class=\"section-title\">Sources (~a web, ~a X posts, ~a tool result rows)</div>\n"
                  (length webs) (length xs) (esc (jget s 'toolResultRows))))
       (W "<div class=\"sources\"><ul>\n")
       (for ([w webs])
         (W (format "<li>~a</li>\n" (link (jget w 'url) (one-line (py-or (jget w 'title) (jget w 'url) ""))))))
       (for ([p xs])
         (define u (format "https://x.com/~a/status/~a" (py-str (jget p 'username)) (py-str (jget p 'postId))))
         (W (format "<li class=\"xpost\">~a: ~a</li>\n"
                    (link u (format "@~a ~a" (py-str (jget p 'username)) (py-str (jget p 'postId))))
                    (esc (one-line (jget p 'text) 300)))))
       (W "</ul></div>\n</section>\n")]))
  (W "</main>\n</body>\n</html>\n")
  (get-output-string out))
