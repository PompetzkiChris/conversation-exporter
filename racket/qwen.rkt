#lang racket/base
;; qwen.rkt — Qwen (chat.qwen.ai, including the Qwen desktop app) lane for the exporter.
;;
;; Where the data comes from: the Qwen desktop app is Electron 35 hosting chat.qwen.ai in a <webview>.
;; Started with --remote-debugging-port=9223 it exposes that signed-in webview over CDP, so the export
;; runs inside the app's own session (token from its localStorage) and nobody signs in anywhere.
;;
;; API lane: GET /api/v2/chats/<id>  (Bearer localStorage.token) ->
;;   data.chat.history.messages   {id -> message}, a tree (parentId / childrenIds; regenerations branch)
;;   data.chat.history.currentId  leaf of the branch the app shows
;;   user:      content, files[{name,url,file_type,size}], models[0], timestamp (s)
;;   assistant: model/modelName, content_list[{phase: thinking_summary | web_search | answer, content,
;;              extra.summary_title.content[], extra.summary_thought.content[], extra.web_search_info[]}],
;;              error {code stage details}  (e.g. data_inspection_failed / stage "output"), usage, timestamp
;;
;; DOM lane: .qwen-chat-message-user / .qwen-chat-message-assistant inside div.chat-messages; replies carry
;; id="chat-response-message-<messageId>".  Old turns are not rendered until the list is scrolled up, so the
;; DOM script scrolls to the top until the counts stop changing (verified 2026-09-14: 10 of 12 rendered at first).

(require racket/string racket/list json)

(provide qwen-input?
         qwen-chat-id
         QWEN-DEBUG-PORT
         QWEN-STATE-JS
         QWEN-FETCH-JS
         QWEN-DOM-JS
         qwen-payload->transcript
         qwen-dom-checks)

(define QWEN-DEBUG-PORT 9223)
(define QWEN-URL-RX #px"^https?://chat[.]qwen[.]ai/c/([0-9a-fA-F-]{36})")

;; "https://chat.qwen.ai/c/<uuid>", or "qwen" / "qwen:current" = the chat open in the Qwen app
(define (qwen-input? s)
  (and (string? s)
       (let ([s (string-trim s)])
         (or (regexp-match? QWEN-URL-RX s)
             (member (string-downcase s) '("qwen" "qwen:current"))))
       #t))
(define (qwen-chat-id s)
  (define m (and (string? s) (regexp-match QWEN-URL-RX (string-trim s))))
  (and m (string-downcase (cadr m))))

;; ------------------------------------------------------------------ JS

(define QWEN-STATE-JS #<<JS
(() => ({href: location.href, host: location.host, title: document.title,
         signedIn: !!localStorage.getItem('token'),
         users: document.querySelectorAll('.qwen-chat-message-user').length,
         assistants: document.querySelectorAll('.qwen-chat-message-assistant').length}))()
JS
  )

(define QWEN-FETCH-JS #<<JS
async (opts) => {
  const token = localStorage.getItem('token');
  if (!token) return {status: 0, text: '', error: 'no localStorage token (not signed in)'};
  const r = await fetch('/api/v2/chats/' + opts.id, {credentials: 'include', headers: {authorization: 'Bearer ' + token}});
  return {status: r.status, text: await r.text()};
}
JS
  )

(define QWEN-DOM-JS #<<JS
async (opts) => {
  const sleep = ms => new Promise(r => setTimeout(r, ms));
  const t0 = Date.now();
  const scroller = () => document.querySelector('div.chat-messages') ||
    [...document.querySelectorAll('*')].filter(e => e.scrollHeight > e.clientHeight + 200 && /auto|scroll/.test(getComputedStyle(e).overflowY))
      .sort((a, b) => b.scrollHeight - a.scrollHeight)[0];
  // only loading indicators inside the chat count: the sidebar keeps skeleton bars visible permanently
  const loading = () => [...((scroller() || document).querySelectorAll('.chat-detail-skeleton-bar, .ant-spin-spinning, [role="progressbar"]'))]
    .some(e => e.offsetParent !== null && e.getBoundingClientRect().height > 0);
  const count = () => document.querySelectorAll('.qwen-chat-message-user, .qwen-chat-message-assistant').length;
  let prev = -1, stable = 0, rounds = 0;
  // scroll:false = read-only snapshot, for a Qwen window someone else (a person or another agent) is using
  while (opts.scroll !== false && rounds < (opts.maxRounds || 900) && Date.now() - t0 < (opts.maxMs || 900000)) {
    rounds++;
    const sc = scroller();
    if (sc) { sc.scrollTop = 0; sc.dispatchEvent(new WheelEvent('wheel', {deltaY: -4000, bubbles: true})); }
    await sleep(opts.waitMs || 1000);
    const n = count();
    const atTop = !sc || sc.scrollTop <= 60;
    if (n === prev && atTop && !loading()) { if (++stable >= (opts.stableRounds || 8)) break; }
    else { stable = 0; prev = n; }
  }
  // Qwen virtualizes the list: only the turns near the viewport exist in the DOM.  Collect what is
  // rendered now, and in scroll mode keep collecting while stepping down to the bottom.
  const norm = s => (s || '').replace(/\u00a0/g, ' ').replace(/\s+/g, ' ').trim();
  const users = [], userSeen = new Set(), assistants = new Map();
  const collect = () => {
    for (const e of document.querySelectorAll('.qwen-chat-message-user')) {
      const t = norm((e.querySelector('.user-message-content') || e).textContent);
      if (!userSeen.has(t)) { userSeen.add(t); users.push(t); }
    }
    for (const e of document.querySelectorAll('.qwen-chat-message-assistant')) {
      const box = e.querySelector('[id^="chat-response-message-"]') || e;
      const id = (box.id || '').replace('chat-response-message-', '');
      // a reply can render several answer blocks (a leaked </think>, then a web search card, then the answer)
      const parts = [...e.querySelectorAll('.response-message-content')];
      const text = norm((parts.length ? parts : [e]).map(p => p.textContent).join(' '));
      if (!assistants.has(id) || text.length > assistants.get(id).length) assistants.set(id, text);
    }
  };
  collect();
  let steps = 0;
  if (opts.scroll !== false) {
    const sc = scroller();
    let still = 0;
    while (sc && steps < 2000 && Date.now() - t0 < (opts.maxMs || 900000)) {
      steps++;
      const before = sc.scrollTop;
      sc.scrollTop = before + Math.max(200, sc.clientHeight * 0.7);
      await sleep(opts.stepMs || 700);
      collect();
      const atBottom = sc.scrollTop + sc.clientHeight >= sc.scrollHeight - 5;
      if (atBottom || sc.scrollTop === before) { if (++still >= 3) break; } else still = 0;
    }
  }
  return {href: location.href, scrolled: opts.scroll !== false, rounds, stableRounds: stable, steps, ms: Date.now() - t0,
          users, assistants: [...assistants].map(([id, text]) => ({id, text}))};
}
JS
  )

;; ------------------------------------------------------------------ transcript

(define (lst v) (if (list? v) v '()))
(define (hget h k [d 'null]) (if (hash? h) (hash-ref h k d) d))
(define (str v [d 'null]) (if (string? v) v d))

(define (epoch->iso sec)
  (cond
    [(real? sec)
     (define s (inexact->exact (floor sec)))
     (define d (seconds->date s #f))
     (define (p2 n) (if (< n 10) (format "0~a" n) (number->string n)))
     (format "~a-~a-~aT~a:~a:~a.000Z" (date-year d) (p2 (date-month d)) (p2 (date-day d))
             (p2 (date-hour d)) (p2 (date-minute d)) (p2 (date-second d)))]
    [else 'null]))

(define (phase-list m) (filter hash? (lst (hget m 'content_list '()))))

(define (thinking-of m)
  (define events
    (append*
     (for/list ([c (phase-list m)])
       (define ex (hget c 'extra (hasheq)))
       (case (hget c 'phase)
         [("thinking_summary")
          (define titles (lst (hget (hget ex 'summary_title (hasheq)) 'content '())))
          (define thoughts (lst (hget (hget ex 'summary_thought (hasheq)) 'content '())))
          (append*
           (for/list ([i (in-range (max (length titles) (length thoughts)))])
             (append (if (and (< i (length titles)) (string? (list-ref titles i)))
                         (list (hasheq 'type "text" 'channel "header" 'text (list-ref titles i))) '())
                     (if (and (< i (length thoughts)) (string? (list-ref thoughts i)))
                         (list (hasheq 'type "text" 'channel "thought" 'text (list-ref thoughts i))) '()))))]
         [("web_search")
          (list (hasheq 'type "tool" 'kind "web_search" 'toolCallId 'null
                        'args (hasheq 'displayPosition (hget ex 'display_position))
                        'results (for/list ([w (lst (hget ex 'web_search_info '()))] #:when (hash? w))
                                   (hasheq 'url (hget w 'url) 'title (hget w 'title)
                                           'snippet (hget w 'snippet) 'date (hget w 'date)))))]
         [else '()]))))
  (if (null? events)
      'null
      (hasheq 'durationMs 'null 'startTime 'null 'endTime 'null 'mainRollout 'null
              'rollouts (list (hasheq 'id "Qwen" 'role 'null 'events events)))))

(define (answer-text m)
  (define answers (for/list ([c (phase-list m)] #:when (equal? (hget c 'phase) "answer")) (str (hget c 'content) "")))
  (cond [(pair? answers) (string-join answers "")]
        [else (str (hget m 'content) "")]))

(define (web-results m)
  (for*/list ([c (phase-list m)] #:when (equal? (hget c 'phase) "web_search")
              [w (lst (hget (hget c 'extra (hasheq)) 'web_search_info '()))] #:when (hash? w))
    (hasheq 'title (hget w 'title) 'url (hget w 'url) 'preview (hget w 'snippet))))

;; payload = parsed JSON of /api/v2/chats/<id>
(define (qwen-payload->transcript payload source-url)
  (define data (hget payload 'data (hasheq)))
  (define hist (hget (hget data 'chat (hasheq)) 'history (hasheq)))
  (define msgs (hget hist 'messages (hasheq)))
  (define by-id (if (hash? msgs) msgs (hasheq)))
  (define (msg id) (and (string? id) (hash-ref by-id (string->symbol id) #f)))
  (define branch
    (let loop ([id (hget hist 'currentId)] [acc '()] [seen (hash)])
      (define m (msg id))
      (if (and m (not (hash-ref seen id #f)))
          (loop (hget m 'parentId) (cons m acc) (hash-set seen id #t))
          acc)))
  (define branch-ids (for/hash ([m branch]) (values (hget m 'id) #t)))
  (define off-branch (for/list ([(k m) (in-hash by-id)] #:unless (hash-ref branch-ids (hget m 'id) #f)) m))
  (define turns
    (for/list ([m branch] [i (in-naturals)])
      (define user? (equal? (hget m 'role) "user"))
      (hasheq 'index i
              'sender (if user? "human" "assistant")
              'text (if user? (str (hget m 'content) "") (answer-text m))
              'createTime (epoch->iso (hget m 'timestamp))
              'responseId (hget m 'id)
              'parentResponseId (hget m 'parentId)
              'model (if user? 'null (or (str (hget m 'modelName) #f) (hget m 'model)))
              'thinking (if user? 'null (thinking-of m))
              'attachments (if user?
                               (for/list ([f (lst (hget m 'files '()))] #:when (hash? f))
                                 (hasheq 'fileId (hget f 'id) 'fileName (hget f 'name) 'url (hget f 'url)
                                         'mimeType (hget f 'file_type) 'sizeBytes (hget f 'size)))
                               '())
              'citations '()
              'sources (if user? 'null (hasheq 'webSearchResults (web-results m)))
              'error (let ([e (hget m 'error)]) (if (hash? e) e 'null))
              'usage (if user? 'null (let ([u (hget m 'usage)]) (if (hash? u) u 'null)))
              'featureConfig (let ([f (hget m 'feature_config)]) (if (hash? f) f 'null))
              'siblings (let ([p (msg (hget m 'parentId))]) (if p (length (lst (hget p 'childrenIds '()))) 1)))))
  (hasheq 'conversation (hasheq 'conversationId (hget data 'id)
                                'title (hget data 'title)
                                'createTime (epoch->iso (hget data 'created_at))
                                'modifyTime (epoch->iso (hget data 'updated_at))
                                'isPublic (if (string? (hget data 'share_id)) #t 'null)
                                'sourceUrl source-url
                                'platform "qwen"
                                'messagesTotal (hash-count by-id)
                                'offBranchMessages (for/list ([m off-branch])
                                                     (hasheq 'id (hget m 'id) 'role (hget m 'role) 'parentId (hget m 'parentId)
                                                             'createTime (epoch->iso (hget m 'timestamp))
                                                             'text (if (equal? (hget m 'role) "user") (str (hget m 'content) "") (answer-text m))
                                                             'error (let ([e (hget m 'error)]) (if (hash? e) e 'null)))))
          'turns turns))

;; ------------------------------------------------------------------ verification

(define NBSP-RX (regexp (string #\[ (integer->char #xA0) #\])))
(define (norm s)
  (define s1 (regexp-replace* #px"[*_`#>|-]" (if (string? s) s "") ""))
  (string-trim (regexp-replace* #px"\\s+" (regexp-replace* NBSP-RX s1 " ") " ")))
(define (prefix s n) (let ([s (norm s)]) (if (> (string-length s) n) (substring s 0 n) s)))
;; a reply that opens with a list item: the page renders the number or bullet as list markup, not as text
(define (strip-list-marker s) (regexp-replace #px"^\\s*(?:[-*+]|[0-9]+[.)])\\s+" s ""))
;; a leaked reasoning delimiter (<think>, </think>, "</think" glued to the answer) is not shown by the page
(define (strip-think s) (regexp-replace* #px"</?think>?" s ""))

;; transcript + DOM capture -> (values checks warnings)
;; Every message the page rendered must match the API (assistant by message id + text, user by text).
;; Coverage (every API message seen on the page) is checked only when the export scrolled a page it owns;
;; a read-only snapshot of a shared window reports how much it showed as a warning, not a failure.
(define (qwen-dom-checks transcript dom)
  (define turns (lst (hash-ref transcript 'turns '())))
  (define humans (filter (lambda (t) (equal? (hash-ref t 'sender) "human")) turns))
  (define assistants (filter (lambda (t) (equal? (hash-ref t 'sender) "assistant")) turns))
  (define du (lst (hget dom 'users '())))
  (define da (lst (hget dom 'assistants '())))
  (define scrolled? (not (eq? (hget dom 'scrolled) #f)))
  (define checks '())
  (define warnings '())
  (define (check! name ok detail [turn 'null])
    (set! checks (cons (hasheq 'name name 'ok (and ok #t) 'detail detail 'turnIndex turn) checks)))
  (define api-by-id (for/hash ([a assistants]) (values (hash-ref a 'responseId) a)))
  (define (first-line s)
    (car (append (filter (lambda (x) (non-empty-string? (norm x))) (regexp-split #px"\n" s)) '(""))))
  ;; rendered assistant messages (the window may show another regenerated version than the API's current branch)
  (define off-by-id (for/hash ([m (lst (hget (hash-ref transcript 'conversation (hasheq)) 'offBranchMessages '()))])
                      (values (hget m 'id) m)))
  (for ([d da])
    (define a (hash-ref api-by-id (hget d 'id) #f))
    (define off (hash-ref off-by-id (hget d 'id) #f))
    (define src (or a off))
    (define want (and src (prefix (strip-list-marker (first-line (strip-think (str (hash-ref src 'text "") "")))) 40)))
    (when (and off (not a))
      (set! warnings (cons (format "the page shows reply ~a, an alternate version of the reply on the API's current branch (parent ~a); both are in the export"
                                   (hget d 'id) (hget off 'parentId))
                           warnings)))
    (check! "page-assistant-in-api"
            (and src (or (string=? want "") (string-contains? (norm (hget d 'text "")) want)))
            (cond [a (format "id ~a; api first line starts ~s" (hget d 'id) want)]
                  [off (format "id ~a (off-branch version); api first line starts ~s" (hget d 'id) want)]
                  [else (format "id ~a not in the API message tree" (hget d 'id))])
            (if a (hash-ref a 'index) 'null)))
  ;; rendered user messages (an image-only message renders as empty text)
  (for ([q du])
    (define h (for/first ([h humans] #:when (let ([w (prefix (hash-ref h 'text) 60)])
                                              (if (string=? (norm q) "")
                                                  (string=? w "")
                                                  (and (non-empty-string? w) (string-contains? (norm q) w)))))
                h))
    (check! "page-user-in-api" (and h #t) (format "page text starts ~s" (prefix q 60)) (if h (hash-ref h 'index) 'null)))
  (cond
    [scrolled?
     (check! "dom-reached-top" (>= (hget dom 'stableRounds 0) 1)
             (format "~a top rounds, ~a steps down, ~a ms" (hget dom 'rounds 0) (hget dom 'steps 0) (hget dom 'ms 0)))
     (define seen (for/hash ([d da]) (values (hget d 'id) #t)))
     (for ([a assistants])
       (check! "api-assistant-seen-on-page" (hash-ref seen (hash-ref a 'responseId) #f)
               (format "message id ~a" (hash-ref a 'responseId)) (hash-ref a 'index)))
     (check! "api-user-count-seen" (>= (length du) (length humans)) (format "page ~a, api ~a" (length du) (length humans)))]
    [else
     (set! warnings (cons (format "read-only snapshot of a shared Qwen window: it showed ~a of ~a replies and ~a of ~a user messages; coverage not checked"
                                  (length da) (length assistants) (length du) (length humans)) warnings))])
  (values (reverse checks) (reverse warnings)))
