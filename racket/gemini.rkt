#lang racket/base
;; gemini.rkt — Gemini (gemini.google.com) lane for the exporter.
;;
;; API lane: the page's own batchexecute RPC `hNvQHb` (conversation history), called from inside the
;; signed-in tab with the page's WIZ_global_data tokens.  Turns arrive newest-first; one call with a
;; page size of 1000 returned all 84 turns of a long chat on 2026-09-14.
;;
;; DOM lane: Gemini lazy-loads old turns and hides them until the chat is scrolled to the very top,
;; so the DOM script scrolls up repeatedly and only stops after the turn count has stayed unchanged
;; for GEMINI-STABLE-ROUNDS consecutive rounds at scrollTop 0 with no loading indicator.
;;
;; Field map (verified against two live chats 2026-09-14):
;;   turn[0] = [cid rid]            turn[1] = [cid rid rcid]        turn[4] = [epochSec nanos]
;;   turn[2][0][0] user text        turn[2][0][4][0][4] attachments [_ _ name url _ token … mime(11)]
;;   turn[2][8]  false / null / absent — recorded raw as inputFlag.  In one test chat false = typed and
;;               null = "Yes" chip, but typed turns with screenshots in another are null too.
;;   turn[3][0]  candidates; turn[3][3] selected rcid; turn[3][1] search queries [[q 1]…];
;;   turn[3][4]  tool usage (Google Search); turn[3][21] model label ("3.1 Pro", "3.1 Deep Think")
;;   cand[1][0] reply markdown; cand[2][1] citations [[snippet _ _ spans] [n] [[url title favicon preview]]];
;;   cand[12] embedded content (YouTube …); cand[37][0][0] stored thinking text.

(require racket/string racket/list racket/port json)

(provide gemini-url?
         gemini-conv-id
         GEMINI-FETCH-JS
         GEMINI-DOM-JS
         GEMINI-STATE-JS
         parse-batchexecute
         gemini-payload->transcript
         gemini-dom-checks)

(define GEMINI-RX #px"^https?://gemini\\.google\\.com/(?:u/\\d+/)?(?:app|gem/[^/]+)/([0-9a-fA-F]{8,})")

(define (gemini-url? s) (and (string? s) (regexp-match? GEMINI-RX (string-trim s))))
(define (gemini-conv-id s)
  (define m (and (string? s) (regexp-match GEMINI-RX (string-trim s))))
  (and m (string-downcase (cadr m))))

;; ------------------------------------------------------------------ JS

;; Page state: is the tab on the conversation and signed in?
(define GEMINI-STATE-JS #<<JS
(() => {
  const w = window.WIZ_global_data || {};
  return {href: location.href, title: document.title, host: location.host,
          signedIn: !!w.SNlM0e, queries: document.querySelectorAll('user-query').length,
          responses: document.querySelectorAll('model-response').length};
})()
JS
  )

;; API lane.  Returns {title, href, pages:[raw text…], errors:[…]}.
(define GEMINI-FETCH-JS #<<JS
async (opts) => {
  const w = window.WIZ_global_data || {};
  const out = {href: location.href, title: document.title.replace(/\s*-\s*Google Gemini\s*$/, ''), pages: [], errors: []};
  if (!w.SNlM0e) { out.errors.push('no WIZ_global_data.SNlM0e (not signed in?)'); return out; }
  const call = async (inner) => {
    const qs = new URLSearchParams({rpcids: 'hNvQHb', 'source-path': location.pathname, bl: w.cfb2h || '',
      'f.sid': w.FdrFJe || '', hl: 'en', _reqid: String(100000 + Math.floor(Math.random() * 900000)), rt: 'c'});
    const r = await fetch('/_/BardChatUi/data/batchexecute?' + qs, {method: 'POST', credentials: 'include',
      headers: {'content-type': 'application/x-www-form-urlencoded;charset=UTF-8'},
      body: new URLSearchParams({'f.req': JSON.stringify([[['hNvQHb', JSON.stringify(inner), null, 'generic']]]), at: w.SNlM0e})});
    return {status: r.status, text: await r.text()};
  };
  let token = null;
  for (let page = 0; page < 50; page++) {
    const res = await call(['c_' + opts.cid, opts.pageSize, token, 1, [1], [4], null, 1]);
    out.pages.push({status: res.status, text: res.text});
    if (res.status !== 200) { out.errors.push('HTTP ' + res.status); break; }
    let next = null;
    try {
      for (const line of res.text.split('\n')) {
        if (!line.startsWith('[')) continue;
        for (const e of JSON.parse(line)) {
          if (e[0] === 'wrb.fr' && e[1] === 'hNvQHb' && typeof e[2] === 'string') {
            const p = JSON.parse(e[2]);
            if (typeof p[1] === 'string' && p[1]) next = p[1];
          }
        }
      }
    } catch (err) { out.errors.push('page ' + page + ' parse: ' + err); }
    if (!next || next === token) break;
    token = next;
  }
  return out;
}
JS
  )

;; DOM lane.  Scroll to the top until nothing new loads, then read every turn.
(define GEMINI-DOM-JS #<<JS
async (opts) => {
  const sleep = ms => new Promise(r => setTimeout(r, ms));
  const t0 = Date.now();
  const scroller = () => [...document.querySelectorAll('*')]
    .filter(e => e.scrollHeight > e.clientHeight + 200 && /auto|scroll/.test(getComputedStyle(e).overflowY))
    .sort((a, b) => b.scrollHeight - a.scrollHeight)[0];
  // Gemini keeps a hidden mat-progress-spinner in the page; only a visible indicator means loading
  const loading = () => [...document.querySelectorAll('mat-progress-spinner, mat-spinner, mat-progress-bar, .loading-history, [role="progressbar"]')]
    .some(e => e.offsetParent !== null && e.getBoundingClientRect().height > 0);
  let prev = -1, stable = 0, rounds = 0, maxRounds = opts.maxRounds || 900;
  while (rounds < maxRounds && Date.now() - t0 < (opts.maxMs || 900000)) {
    rounds++;
    const sc = scroller();
    if (sc) {
      sc.scrollTop = 0;
      sc.dispatchEvent(new WheelEvent('wheel', {deltaY: -4000, bubbles: true}));
    }
    await sleep(opts.waitMs || 1000);
    const n = document.querySelectorAll('user-query').length;
    const atTop = !sc || sc.scrollTop <= 60;   // the chat rests at ~16 px, not 0
    if (n === prev && atTop && !loading()) { if (++stable >= (opts.stableRounds || 8)) break; }
    else { stable = 0; prev = n; }
  }
  const norm = s => (s || '').replace(/ /g, ' ').replace(/\s+/g, ' ').trim();
  const queries = [...document.querySelectorAll('user-query')].map(e => norm(e.textContent));
  const responses = [...document.querySelectorAll('model-response')].map(e => ({
    text: norm(e.textContent),
    thoughts: !!e.querySelector('model-thoughts, [class*="thought"]')
  }));
  return {rounds, stableRounds: stable, ms: Date.now() - t0, queries, responses};
}
JS
  )

;; ------------------------------------------------------------------ batchexecute

;; Raw `rt=c` body -> the hNvQHb payload jsexpr, or #f.
(define (parse-batchexecute raw)
  (define in (open-input-string raw))
  (let loop ([found #f])
    (define line (read-line in 'any))
    (cond
      [(eof-object? line) found]
      [(string-prefix? (string-trim line) "[")
       (define v (with-handlers ([exn:fail? (lambda (e) #f)]) (string->jsexpr (string-trim line))))
       (define p
         (and (list? v)
              (for/or ([e v])
                (and (list? e) (>= (length e) 3)
                     (equal? (car e) "wrb.fr") (equal? (cadr e) "hNvQHb") (string? (caddr e))
                     (with-handlers ([exn:fail? (lambda (x) #f)]) (string->jsexpr (caddr e)))))))
       (loop (or found p))]
      [else (loop found)])))

;; ------------------------------------------------------------------ transcript

(define (at v . path)
  (for/fold ([v v]) ([k path])
    (cond [(and (list? v) (exact-nonnegative-integer? k) (< k (length v))) (list-ref v k)]
          [else 'null])))
(define (str-or v [d 'null]) (if (string? v) v d))
(define (lst v) (if (list? v) v '()))

(define (epoch->iso sec nanos)
  (cond
    [(exact-integer? sec)
     (define d (seconds->date sec #f))
     (format "~a-~a-~aT~a:~a:~a.~aZ"
             (date-year d) (pad2 (date-month d)) (pad2 (date-day d))
             (pad2 (date-hour d)) (pad2 (date-minute d)) (pad2 (date-second d))
             (let ([ms (if (exact-integer? nanos) (quotient nanos 1000000) 0)])
               (string-append (make-string (- 3 (string-length (number->string ms))) #\0) (number->string ms))))]
    [else 'null]))
(define (pad2 n) (if (< n 10) (format "0~a" n) (number->string n)))

;; "**Title**\n\nbody\n\n**Title2**…" -> header/text events
(define (thinking->events s)
  (define parts (regexp-split #px"\n{2,}" (string-trim s)))
  (for/list ([p parts] #:when (non-empty-string? (string-trim p)))
    (define m (regexp-match #px"^\\*\\*(.+?)\\*\\*$" (string-trim p)))
    (if m
        (hasheq 'type "text" 'channel "header" 'text (cadr m))
        (hasheq 'type "text" 'channel "thought" 'text (string-trim p)))))

(define (attachments-of user)
  (for/list ([a (lst (at user 0 4 0 4))] #:when (list? a))
    (hasheq 'fileName (str-or (at a 2)) 'url (str-or (at a 3)) 'mimeType (str-or (at a 11))
            'fileId (str-or (at a 2)))))

(define (citations-of cand)
  (for*/list ([c (lst (at cand 2 1))] #:when (list? c)
              [src (lst (at c 2))] #:when (list? src))
    (define spans (lst (at c 0 3)))
    (hasheq 'text (str-or (at c 0 0))
            'url (str-or (at src 0))
            'title (str-or (at src 1))
            'preview (str-or (at src 3))
            'start (let ([v (at spans 0 0)]) (if (exact-integer? v) v 'null))
            'end (let ([v (at spans 0 1)]) (if (exact-integer? v) v 'null)))))

(define (embeds-of cand)
  (define acc '())
  (let walk ([v (at cand 12)])
    (cond
      [(and (list? v) (>= (length v) 3) (string? (at v 0)) (string? (at v 2))
            (regexp-match? #px"^https?://" (at v 2)))
       (set! acc (cons (hasheq 'title (at v 0) 'id (str-or (at v 1)) 'url (at v 2) 'channel (str-or (at v 3))) acc))]
      [(list? v) (for-each walk v)]
      [(hash? v) (for ([x (in-hash-values v)]) (walk x))]
      [else (void)]))
  (remove-duplicates (reverse acc)))

(define (gemini-payload->transcript payloads source-url title)
  (define turns-newest-first (append* (for/list ([p payloads]) (lst (at p 0)))))
  (define api-turns (reverse turns-newest-first))
  (define cid (for/or ([t api-turns]) (let ([v (at t 0 0)]) (and (string? v) v))))
  (define out
    (for/fold ([acc '()] #:result (reverse acc)) ([t api-turns])
      (define rid (str-or (at t 0 1)))
      (define ts (epoch->iso (at t 4 0) (at t 4 1)))
      (define user (at t 2))
      (define chip-flag (at user 8))
      (define human
        (hasheq 'index (length acc) 'sender "human" 'text (str-or (at user 0 0) "")
                'createTime ts 'responseId rid 'parentResponseId 'null 'model 'null 'thinking 'null
                'attachments (attachments-of user) 'citations '() 'sources 'null
                ;; raw turn[2][8]: "false" | "null" | "absent".  Not a reliable chip marker: typed messages
                ;; with screenshots on 2026-09-01 carry null too (see gemini-capture evidence file).
                'inputFlag (cond [(<= (length (lst user)) 8) "absent"]
                                 [(eq? chip-flag #f) "false"]
                                 [(eq? chip-flag 'null) "null"]
                                 [else (jsexpr->string chip-flag)])))
      (define cands (lst (at t 3 0)))
      (define sel-id (at t 3 3))
      (define cand (or (for/or ([c cands]) (and (equal? (at c 0) sel-id) c)) (and (pair? cands) (car cands))))
      (define think (str-or (at cand 37 0 0) #f))
      (define assistant
        (hasheq 'index (add1 (length acc)) 'sender "assistant"
                'text (str-or (at cand 1 0) "")
                'createTime ts
                'responseId (str-or (at cand 0))
                'parentResponseId rid
                'model (str-or (at t 3 21))
                'thinking (if think
                              (hasheq 'durationMs 'null 'startTime 'null 'endTime 'null 'mainRollout 'null
                                      'text think
                                      'rollouts (list (hasheq 'id "Gemini" 'role 'null 'events (thinking->events think))))
                              'null)
                'attachments '()
                'citations (citations-of cand)
                'sources (hasheq 'searchQueries (for/list ([q (lst (at t 3 1))] #:when (string? (at q 0))) (at q 0))
                                 'webSearchResults (remove-duplicates
                                                    (for/list ([c (citations-of cand)] #:when (string? (hash-ref c 'url)))
                                                      (hasheq 'title (hash-ref c 'title) 'url (hash-ref c 'url)
                                                              'preview (hash-ref c 'preview))))
                                 'embeds (embeds-of cand))
                'candidates (length cands)))
      (list* assistant human acc)))
  (hasheq 'conversation (hasheq 'conversationId (or cid 'null)
                                'title (or title 'null)
                                'createTime (if (pair? out) (hash-ref (car out) 'createTime) 'null)
                                'modifyTime (if (pair? out) (hash-ref (last out) 'createTime) 'null)
                                'isPublic 'null
                                'sourceUrl source-url
                                'platform "gemini")
          'turns out))

;; ------------------------------------------------------------------ verification

;; NBSP (U+00A0) is common in Gemini user text; the page read already turns it into a space
(define (norm s)
  (define s1 (regexp-replace* #px"[*_`#>]" (if (string? s) s "") ""))
  (define s2 (regexp-replace* (regexp (string #\[ (integer->char #xA0) #\])) s1 " "))
  (string-trim (regexp-replace* #px"\\s+" s2 " ")))
(define (prefix s n) (let ([s (norm s)]) (if (> (string-length s) n) (substring s 0 n) s)))
;; a reply that opens with a list item: the page renders the number or bullet as list markup, not as text
(define (strip-list-marker s) (regexp-replace #px"^\\s*(?:[-*+]|[0-9]+[.)])\\s+" s ""))

;; transcript + DOM capture -> (values checks warnings)
(define (gemini-dom-checks transcript dom)
  (define turns (lst (hash-ref transcript 'turns '())))
  (define humans (filter (lambda (t) (equal? (hash-ref t 'sender) "human")) turns))
  (define assistants (filter (lambda (t) (equal? (hash-ref t 'sender) "assistant")) turns))
  (define dq (lst (hash-ref dom 'queries '())))
  (define dr (lst (hash-ref dom 'responses '())))
  (define checks '())
  (define (check! name ok detail [turn 'null])
    (set! checks (cons (hasheq 'name name 'ok ok 'detail detail 'turnIndex turn) checks)))
  (check! "dom-reached-top" (>= (hash-ref dom 'stableRounds 0) 1)
          (format "~a scroll rounds, stable ~a, ~a ms" (hash-ref dom 'rounds 0) (hash-ref dom 'stableRounds 0) (hash-ref dom 'ms 0)))
  (check! "human-turn-count" (= (length dq) (length humans)) (format "page ~a, api ~a" (length dq) (length humans)))
  (check! "assistant-turn-count" (= (length dr) (length assistants)) (format "page ~a, api ~a" (length dr) (length assistants)))
  ;; align from the newest end: if the page still hides old turns, the newest ones must still match
  (define k (min (length dq) (length humans)))
  (for ([i (in-range k)])
    (define h (list-ref humans (- (length humans) 1 i)))
    (define q (list-ref dq (- (length dq) 1 i)))
    (define want (prefix (hash-ref h 'text) 60))
    (check! "human-text" (string-contains? (norm q) want) (format "api starts ~s" want) (hash-ref h 'index)))
  (define k2 (min (length dr) (length assistants)))
  (for ([i (in-range k2)])
    (define a (list-ref assistants (- (length assistants) 1 i)))
    (define r (list-ref dr (- (length dr) 1 i)))
    (define want (prefix (strip-list-marker (car (append (filter non-empty-string? (map string-trim (regexp-split #px"\n" (hash-ref a 'text)))) '("")))) 40))
    (check! "assistant-text" (or (string=? want "") (string-contains? (norm (hash-ref r 'text "")) want))
            (format "api first line starts ~s" want) (hash-ref a 'index)))
  (values (reverse checks) '()))
