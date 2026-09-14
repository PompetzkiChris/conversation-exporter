#lang racket/base
;; gui.rkt — Grok Exporter · Claude Version · Racket
;;
;; A reader, not a form.  The left rail lists every conversation exported to disk; selecting one
;; renders it in full — your messages and attachments, each agent's thoughts with its tool calls,
;; the reply, its citations and sources — with everything collapsible.  Exporting is one bar at
;; the top; while a run is going its log streams in the reading pane and the finished export is
;; selected when it lands.
;;
;; Every pixel is drawn by this module (no native controls).  The document model lives in
;; reader.rkt, which is pure data and testable without a window.  All export work happens in
;; gui-run.rkt on a worker thread.  The previous form-shaped window is kept as gui.rkt.form-version.
(require racket/gui/base
         racket/class
         racket/draw
         racket/string
         racket/list
         racket/math
         racket/file
         racket/path
         racket/async-channel
         ffi/unsafe
         net/sendurl
         "gui-run.rkt"
         "reader.rkt"
         "analysis.rkt")

(define APP-NAME "Exporter")
(define APP-EDITION "Herr Pompetzki und Signore Amodei (lol)")
(define WINDOW-TITLE "Exporter - Herr Pompetzki und Signore Amodei (lol)")

;; ============================================================================ theme
(define (C r g b) (make-object color% r g b))
(define BG        (C 246 245 242))   ; the reading surface: paper
(define PAPER     (C 252 251 249))
(define RAIL      (C  35  37  40))   ; the chrome stays dark and quiet
(define RAIL-HI   (C  46  49  53))
(define CARD      (C 240 238 234))
(define CARD-HI   (C 234 232 227))
(define INPUT-BG  (C  28  30  33))
(define LINE      (C 214 211 205))
(define LINE-HI   (C 188 184 176))
(define RULE      (C  60  63  67))
(define TXT       (C  26  27  29))   ; document text
(define DIM       (C  92  95  99))
(define FAINT     (C 132 135 139))
(define GHOST     (C 168 170 173))
(define RTXT      (C 224 226 229))   ; text on the dark chrome
(define RDIM      (C 150 154 159))
(define RFAINT    (C 108 112 117))
(define ACCENT    (C  94 108 128))
(define ACCENT2   ACCENT)
(define OK        (C  62 104  72))
(define WARN      (C 150 106  30))
(define BAD       (C 150  54  50))
(define USERBG    (C 236 234 229))
(define CODEBG    (C 243 241 237))
(define LINKC     (C  42  74 122))

(define UI-FACE
  (let ([f (get-face-list)])
    (cond [(member "Segoe UI Variable Text" f) "Segoe UI Variable Text"]
          [(member "Segoe UI" f) "Segoe UI"] [else "Arial"])))
(define MONO-FACE
  (let ([f (get-face-list)])
    (cond [(member "Cascadia Mono" f) "Cascadia Mono"]
          [(member "Consolas" f) "Consolas"] [else "Courier New"])))
(define DOC-FACE
  (let ([f (get-face-list)])
    (cond [(member "Constantia" f) "Constantia"]
          [(member "Cambria" f) "Cambria"]
          [(member "Georgia" f) "Georgia"] [else "Times New Roman"])))
(define (doc n [w (quote normal)] [s (quote normal)])
  (make-font #:face DOC-FACE #:size n #:weight w #:style s #:smoothing (quote smoothed)))
(define (ui n [w 'normal]) (make-font #:face UI-FACE #:size n #:weight w #:smoothing 'smoothed))
(define (mono n) (make-font #:face MONO-FACE #:size n #:smoothing 'smoothed))

(define F-BRAND  (ui 13 'bold))
(define F-TITLE  (doc 19 'bold))
(define F-H2     (doc 12 'bold))
(define F-BODY   (ui 10))
(define F-READ   (doc 12))
(define F-SMALL  (ui  9))
(define F-DATA   (mono 8))
(define F-DATAB  (mono 9))
(define F-TINY   (ui  8))
(define F-LABEL  (ui  8 'bold))
(define F-BTN    (ui 10 'bold))
(define F-MONO   (mono 9))

;; ============================================================================ draw helpers
(define (no-pen dc) (send dc set-pen (make-pen #:style 'transparent)))
(define (solid dc c) (no-pen dc) (send dc set-brush (make-brush #:color c)))
(define (stroke dc c [w 1]) (send dc set-pen (make-pen #:color c #:width w))
                            (send dc set-brush (make-brush #:style 'transparent)))
(define (fill-rect dc x y w h c) (when (and (> w 0) (> h 0)) (solid dc c) (send dc draw-rectangle x y w h)))
(define (fill-round dc x y w h r c)
  (when (and (> w 0) (> h 0)) (solid dc c) (send dc draw-rounded-rectangle x y w h r)))
;; Flat fills only: a gradient on a control reads as decoration, and this is a tool.
(define (fill-grad dc x y w h r c1 c2 [horiz #t])
  (fill-round dc x y w h r c1))
(define (outline dc x y w h r c [width 1])
  (when (and (> w 0) (> h 0)) (stroke dc c width) (send dc draw-rounded-rectangle x y w h r)))
(define (text-at dc s x y f c)
  (send dc set-font f) (send dc set-text-foreground c)
  (send dc draw-text s (inexact->exact (round x)) (inexact->exact (round y)) #t))
(define (tsize dc s f)
  (define-values (w h d a) (send dc get-text-extent s f #t)) (values w h))
(define (tw dc s f) (define-values (w h) (tsize dc s f)) w)
(define (tcenter dc s x y w h f c)
  (define-values (a b) (tsize dc s f)) (text-at dc s (+ x (/ (- w a) 2)) (+ y (/ (- h b) 2)) f c))
;; Longest prefix that fits with the ellipsis, found by bisection: a character-at-a-time scan
;; measured a 300-character source title up to 300 times.
(define (elide dc s f maxw)
  (if (<= (tw dc s f) maxw) s
      (let loop ([lo 1] [hi (sub1 (string-length s))])
        (cond [(> lo hi)
               (if (<= hi 1) "…" (string-append (substring s 0 hi) "…"))]
              [else
               (define mid (quotient (+ lo hi) 2))
               (if (<= (tw dc (string-append (substring s 0 mid) "…") f) maxw)
                   (loop (add1 mid) hi)
                   (loop lo (sub1 mid)))]))))

;; word wrap, cached ---------------------------------------------------------
;; Measuring the whole candidate line after every word made the first layout of a 279-turn
;; record take 1.75 s.  Word widths are cached per font and summed; the line is measured for
;; real only once the running sum passes 85% of the width, so every break decision is still
;; made on an exact extent and short lines cost one lookup per word.
(define wrap-cache (make-hash))
(define word-width-cache (make-hash))
(define (word-w dc w f) (hash-ref! word-width-cache (cons f w) (lambda () (tw dc w f))))
(define (space-w dc f)
  (hash-ref! word-width-cache (cons f 'space)
             (lambda () (max 0 (- (tw dc "n n" f) (* 2 (tw dc "n" f)))))))
(define (wrap dc s f width)
  (define key (list s (send f get-face) (send f get-point-size) (send f get-weight)
                    (inexact->exact (round width))))
  (hash-ref! wrap-cache key
             (lambda ()
               (cond
                 [(<= width 8) (list s)]
                 [else
                  (define words (string-split s " "))
                  (define sp (space-w dc f))
                  (define near (* 0.85 width))
                  (cond
                    [(null? words) (list "")]
                    [else
                     ;; cur-w: width of cur, exact once measured, otherwise the running sum
                     (let loop ([ws (cdr words)] [cur (car words)] [cur-w (word-w dc (car words) f)] [acc '()])
                       (cond
                         [(null? ws) (reverse (cons cur acc))]
                         [else
                          (define w (car ws))
                          (define est (+ cur-w sp (word-w dc w f)))
                          (define try-w (if (<= est near) est (tw dc (string-append cur " " w) f)))
                          (if (<= try-w width)
                              (loop (cdr ws) (string-append cur " " w) try-w acc)
                              (loop (cdr ws) w (word-w dc w f) (cons cur acc)))]))])]))))

;; Evidence quotes are exact spans of the transcript, so they keep its line breaks and Grok's
;; inline image markup.  `wrap` splits on spaces only, so a "word" holding a newline went to
;; draw-text as one run: in the reader the opening of each paragraph was missing and the image
;; XML was printed.  Break on the newlines first and show each image card as a marker.
(define (evidence-display s)
  (regexp-replace* #px"<grok:render\\b[^>]*>.*?</grok:render>" (or s "") "[image]"))
(define (wrap-lines dc s f width)
  (for*/list ([para (in-list (string-split (evidence-display s) "\n" #:trim? #f))]
              #:unless (string=? (string-trim para) "")
              [l (in-list (wrap dc (string-trim para) f width))])
    l))

;; ============================================================================ widgets
(struct rct (x y w h) #:transparent)
(define (inside? r mx my)
  (and r (>= mx (rct-x r)) (< mx (+ (rct-x r) (rct-w r)))
       (>= my (rct-y r)) (< my (+ (rct-y r) (rct-h r)))))

(struct btn ([r #:mutable] label kind [on? #:mutable] act [hov #:mutable] [dn #:mutable]) #:transparent)
(define (mk-btn label kind act) (btn (rct 0 0 0 0) label kind #t act #f #f))

(struct fld ([r #:mutable] [text #:mutable] ph [caret #:mutable] [sel #:mutable]
             [scroll #:mutable] [foc #:mutable] [hov #:mutable] num?) #:transparent)
(define (mk-fld text ph #:num? [num? #f]) (fld (rct 0 0 0 0) text ph (string-length text) #f 0 #f #f num?))

(struct seg ([r #:mutable] opts [idx #:mutable] [hov #:mutable] [cells #:mutable]) #:transparent)
(define (mk-seg opts idx) (seg (rct 0 0 0 0) opts idx -1 '()))

;; ============================================================================ state
(define state 'idle)            ; 'idle 'running 'cancelling 'done
(define view 'reader)           ; 'reader | 'log
(define log-lines '()) (define log-count 0) (define log-scroll 0) (define log-follow? #t)
(define phase-text "") (define phase-frac 0.0) (define phase-shown 0.0)
(define current-job #f) (define result #f) (define result-exit 0)
(define export-dir #f) (define login-note #f)
(define spinner 0.0) (define started-ms 0) (define elapsed-ms 0)
(define toast #f) (define toast-until 0)

(define library '())            ; list of export-entry, newest first
(define lib-filter "")
(define selected #f)            ; export-entry
(define convo #f)               ; jsexpr
(define blocks '())             ; list of block
(define open-tbl (make-hash))   ; group key -> boolean
(define parent-map (make-hash)) ; group key -> parent key
(define laid '())               ; vectors: kind block lines y h indent extra
(define doc-height 0)
(define doc-scroll 0)
(define rail-scroll 0)
(define layout-width -1)
(define thumb-cache (make-hash))
(define link-rects '())
(define group-rects '())
(define rail-rects '())
(define last-hover-key #f)

(define (log! s)
  (set! log-lines (append log-lines (list s))) (set! log-count (add1 log-count))
  (when (> log-count 3000) (set! log-lines (list-tail log-lines 800)) (set! log-count (- log-count 800)))
  (when log-follow? (set! log-scroll 0)))

;; ============================================================================ controls
(define f-url (mk-fld "" "Paste a Grok, Gemini or Qwen conversation link  (or type qwen = the chat open in the Qwen app)"))
(define f-out-dir (mk-fld (default-out-dir) ""))
(define f-search (mk-fld "" "Search"))
;; Turbo = API only, transcript + reasoning + sources, no attachment downloads, no DOM (~11s).
;; Full  = Turbo plus every attachment fetched. Verified = Full plus the DOM cross-check.
(define s-mode (mk-seg '("Turbo" "Full" "Verified") 0))
(define b-export (mk-btn "Export" 'primary (lambda () (do-export))))
(define b-open (mk-btn "Open folder" 'ghost
                       (lambda () (when selected (open-path (path->string (export-entry-dir selected)))))))
(define b-html (mk-btn "Open HTML" 'ghost
                       (lambda () (when selected
                                    (let ([p (build-path (export-entry-dir selected) "transcript.html")])
                                      (when (file-exists? p) (send-url/file p)))))))
(define b-behavior (mk-btn "Behavior report" 'ghost
                           (lambda () (when selected
                                        (let ([p (build-path (export-entry-dir selected)
                                                             "behavior-report.md")])
                                          (when (file-exists? p) (open-path p)))))))
(define all-btns (list b-export b-open b-html b-behavior))
(define all-flds (list f-url f-search))

;; ============================================================================ geometry
(define TOPH 62)
(define RAILW 250)
(define PADX 30)
(define the-frame #f) (define the-canvas #f)
(define (repaint!) (when the-canvas (send the-canvas refresh)))

;; ============================================================================ library
(define (visible-library)
  (define q (string-downcase (string-trim lib-filter)))
  (if (string=? q "") library
      (filter (lambda (e) (or (string-contains? (string-downcase (export-entry-title e)) q)
                              (string-contains? (string-downcase (export-entry-mode e)) q)))
              library)))

(define (group-open? k) (hash-ref open-tbl k #f))
(define (parent-of k) (hash-ref parent-map k #f))
(define (visible-block? b)
  (let loop ([p (block-parent b)])
    (cond [(not p) #t] [(group-open? p) (loop (parent-of p))] [else #f])))

(define (select-export! e)
  (set! selected e)
  (set-btn-on?! b-behavior
                (file-exists? (build-path (export-entry-dir e) "behavior-report.md")))
  (set! convo (with-handlers ([exn:fail? (lambda (ex) #f)]) (load-conversation (export-entry-dir e))))
  (set! blocks (if convo (conversation-blocks convo) '()))
  (hash-clear! open-tbl)
  (for ([b (in-list blocks)]) (when (and (block-key b) (block-open? b)) (hash-set! open-tbl (block-key b) #t)))
  (set! doc-scroll 0) (set! layout-width -1) (set! view 'reader)
  (set! thumb-cache (make-hash)))

(define (refresh-library!)
  (set! library (with-handlers ([exn:fail? (lambda (e) '())]) (scan-exports (fld-text f-out-dir))))
  (when (and (not selected) (pair? library)) (select-export! (car library))))

;; ============================================================================ document layout
(define (relayout! dc width)
  (hash-clear! parent-map)
  (for ([b (in-list blocks)]) (when (block-key b) (hash-set! parent-map (block-key b) (block-parent b))))
  (define items '())
  (define y 0)
  (define (push! kind b lines h ind)
    (set! items (cons (vector kind b lines y h ind) items))
    (set! y (+ y h)))
  (for ([b (in-list blocks)])
    (when (visible-block? b)
      (define ind (block-indent b))
      (define x0 (+ PADX (* ind 22)))
      (define avail (max 60 (- width x0 PADX)))
      (case (block-kind b)
        [(title) (push! 'title b (list (block-text b)) 46 ind)]
        [(meta)  (push! 'meta b (list (block-text b)) 30 ind)]
        [(rule)  (push! 'rule b '() 26 ind)]
        [(user)
         (define w (min avail 640))
         (define ls (wrap dc (block-text b) F-READ (- w 32)))
         (push! 'user b ls (+ 22 (* (length ls) 22)) ind)]
        [(reply)
         (define ls (wrap dc (block-text b) F-READ (min avail 760)))
         (push! 'reply b ls (+ 10 (* (length ls) 23)) ind)]
        [(head)
         (define ls (wrap dc (block-text b) F-H2 (min avail 760)))
         (push! 'head b ls (+ 18 (* (length ls) 24)) ind)]
        [(bullet)
         (define ls (wrap dc (block-text b) F-READ (- (min avail 760) 26)))
         (push! 'bullet b ls (+ 8 (* (length ls) 23)) ind)]
        [(quote)
         (define ls (wrap dc (block-text b) F-READ (- (min avail 740) 20)))
         (push! 'quote b ls (+ 12 (* (length ls) 23)) ind)]
        ;; Rendered thumbnails may be as tall as 150 px plus their 12 px frame.
        ;; Reserve the full painted height so the following user bubble cannot
        ;; overlap the attachment.
        [(image) (push! 'image b (list (block-text b)) 170 ind)]
        [(group) (push! 'group b (list (block-text b)) (if (= ind 0) 38 30) ind)]
        [(flaggroup behaviorgroup) (push! (block-kind b) b (list (block-text b)) 34 ind)]
        [(dossier) (push! 'dossier b (list (block-text b)) 34 ind)]
        [(tally)   (push! 'tally b (list (block-text b)) 22 ind)]
        [(finding behaviorfinding)
         (define ex (block-extra b))
         (define quote-txt (if (and (list? ex) (>= (length ex) 3)) (list-ref ex 2) ""))
         (define behavior? (eq? (block-kind b) 'behaviorfinding))
         (define quote-txt* (if behavior?
                                (if (and (list? ex) (>= (length ex) 4))
                                    (list-ref ex 3)
                                    "")
                                quote-txt))
         (define note-txt  (if (and (list? ex) (>= (length ex) (if behavior? 5 4)))
                               (list-ref ex (if behavior? 4 3)) ""))
         (define ref-txt   (if (and (list? ex) (>= (length ex) (if behavior? 6 5)))
                               (list-ref ex (if behavior? 5 4)) ""))
         (define w (max 120 (- (min avail 780) 16)))
         (define ql (wrap-lines dc quote-txt* F-MONO (- w 20)))
         (define nl (wrap-lines dc note-txt F-SMALL (- w 20)))
         (push! (block-kind b) b (append (list ref-txt) ql (list "") nl)
                (+ 34 (* (length ql) 15) (* (length nl) 17) 14) ind)]
        [(summary)
         (define ls (wrap dc (block-text b) F-BODY (- avail 20)))
         (push! 'summary b ls (+ 4 (* (length ls) 19)) ind)]
        [(tool)  (push! 'tool b (list (block-text b)) 24 ind)]
        ;; one row each; the painter elides to the width, so wrapping the whole title only to
        ;; keep its first line was layout work with nothing drawn from it
        [(result source)
         (define s (block-text b))
         (define shown (hash-ref! wrap-cache (list 'elide s (inexact->exact (round (- avail 10))))
                                  (lambda () (elide dc s F-BODY (- avail 10)))))
         (push! (block-kind b) b (list shown) 22 ind)]
        [(code)
         (define mono? (eq? (block-extra b) 'mono))
         (define f (if mono? F-MONO F-BODY))
         (define lh (if mono? 16 18))
         (define raw (string-split (block-text b) "\n"))
         (define ls (for*/list ([l (in-list raw)]
                                [piece (in-list (if (> (tw dc l f) (- avail 24))
                                                    (wrap dc l f (- avail 24))
                                                    (list l)))])
                      piece))
         (define shown (if (> (length ls) 400) (append (take ls 400) (list "…")) ls))
         (push! 'code b shown (+ 20 (* (max 1 (length shown)) lh)) ind)]
        [else (push! 'other b (list (block-text b)) 22 ind)])))
  (set! laid (reverse items))
  (set! doc-height (+ y 80))
  (set! layout-width width))

;; ============================================================================ painting
(define (export-label) (case state [(running) "Cancel"] [(cancelling) "…"] [else "Export"]))

(define (paint-field dc f)
  (define r (fld-r f))
  (define x (rct-x r)) (define y (rct-y r)) (define w (rct-w r)) (define h (rct-h r))
  (fill-round dc x y w h 0 INPUT-BG)
  (outline dc x y w h 8 (cond [(fld-foc f) ACCENT] [(fld-hov f) LINE-HI] [else LINE]) (if (fld-foc f) 2 1))
  (define ix (+ x 12)) (define iw (- w 24))
  (send dc set-clipping-rect ix y iw h)
  (define t (fld-text f))
  (define-values (mw mh) (tsize dc "M" F-BODY))
  (define ty (+ y (/ (- h mh) 2)))
  (if (and (string=? t "") (not (fld-foc f)))
      (text-at dc (elide dc (fld-ph f) F-BODY iw) ix ty F-BODY RFAINT)
      (text-at dc t (- ix (fld-scroll f)) ty F-BODY RTXT))
  (when (and (fld-foc f) (< (modulo (inexact->exact (floor (/ (current-inexact-milliseconds) 530))) 2) 1))
    (define cx (- (+ ix (tw dc (substring t 0 (min (fld-caret f) (string-length t))) F-BODY)) (fld-scroll f)))
    (stroke dc RTXT 1) (send dc draw-line cx (+ y 7) cx (+ y h -7)))
  (send dc set-clipping-region #f))

(define (paint-seg dc s)
  (define r (seg-r s))
  (define x (rct-x r)) (define y (rct-y r)) (define w (rct-w r)) (define h (rct-h r))
  (fill-round dc x y w h 0 INPUT-BG) (outline dc x y w h 0 LINE)
  (define n (length (seg-opts s)))
  (define cw (/ (- w 6) n))
  (set-seg-cells! s (for/list ([i (in-range n)]) (rct (+ x 3 (* i cw)) (+ y 3) cw (- h 6))))
  (for ([o (seg-opts s)] [i (in-naturals)])
    (define c (list-ref (seg-cells s) i))
    (when (= i (seg-idx s)) (fill-round dc (rct-x c) (rct-y c) (rct-w c) (rct-h c) 0 RAIL-HI))
    (tcenter dc o (rct-x c) (rct-y c) (rct-w c) (rct-h c) F-BTN
             (cond [(= i (seg-idx s)) RTXT] [(= i (seg-hov s)) RDIM] [else RFAINT]))))

(define (paint-btn dc b)
  (define r (btn-r b))
  (when (> (rct-w r) 0)
    (define x (rct-x r)) (define y (rct-y r)) (define w (rct-w r)) (define h (rct-h r))
    (case (btn-kind b)
      [(primary)
       (cond
         [(not (btn-on? b)) (fill-round dc x y w h 0 RAIL-HI)]
         [(btn-dn b)  (fill-round dc x y w h 0 (C 210 212 215))]
         [(btn-hov b) (fill-round dc x y w h 0 (C 236 238 240))]
         [else        (fill-round dc x y w h 0 (C 224 226 229))])
       (tcenter dc (export-label) x y w h F-BTN (if (btn-on? b) (C 24 26 28) RFAINT))]
      [else
       (when (btn-hov b) (fill-round dc x y w h 0 RAIL-HI))
       (tcenter dc (btn-label b) x y w h F-SMALL (if (btn-hov b) RTXT RDIM))])))

(define (paint-topbar dc W)
  (fill-rect dc 0 0 W TOPH RAIL)
  (stroke dc RULE 1) (send dc draw-line 0 (sub1 TOPH) W (sub1 TOPH))
  (text-at dc "GROK EXPORTER" 24 16 F-BRAND RTXT)
  (text-at dc "CONVERSATION ARCHIVE  ·  CLAUDE / RACKET" 24 36 F-DATA RFAINT)
  (define bw 112) (define sw 150) (define gap 12) (define ux 250)
  (define uw (max 180 (- W ux bw sw (* 3 gap) 20)))
  (set-fld-r! f-url (rct ux 15 uw 32)) (paint-field dc f-url)
  (set-seg-r! s-mode (rct (+ ux uw gap) 15 sw 32)) (paint-seg dc s-mode)
  (set-btn-r! b-export (rct (+ ux uw gap sw gap) 15 bw 32)) (paint-btn dc b-export))

(define (paint-rail dc H)
  (fill-rect dc 0 TOPH RAILW (- H TOPH) RAIL)
  (stroke dc RULE 1) (send dc draw-line RAILW TOPH RAILW H)
  (define vis (visible-library))
  (text-at dc "RECORDS" 18 (+ TOPH 14) F-LABEL RDIM)
  (define cnt (format "~a REC" (length vis)))
  (text-at dc cnt (- RAILW 18 (tw dc cnt F-DATA)) (+ TOPH 15) F-DATA RFAINT)
  (set-fld-r! f-search (rct 16 (+ TOPH 34) (- RAILW 32) 28)) (paint-field dc f-search)
  (define y0 (+ TOPH 74))
  (send dc set-clipping-rect 0 y0 RAILW (- H y0))
  (set! rail-rects '())
  (for ([e (in-list vis)] [i (in-naturals)])
    (define y (- (+ y0 (* i 62)) rail-scroll))
    (when (and (> (+ y 62) y0) (< y H))
      (define sel? (and selected (equal? (export-entry-dir e) (export-entry-dir selected))))
      (when sel?
        (fill-round dc 0 y RAILW 56 0 RAIL-HI)
        (solid dc RTXT) (send dc draw-rectangle 0 y 3 56))
      (text-at dc (elide dc (export-entry-title e) F-BODY (- RAILW 44)) 18 (+ y 7) F-BODY (if sel? RTXT RDIM))
      (text-at dc (format "~a TURNS   ~a" (export-entry-turns e) (string-upcase (export-entry-mode e)))
               18 (+ y 25) F-DATA RFAINT)
      (text-at dc (let* ([d (path->string (export-entry-dir e))]
                         [m (regexp-match #px"([0-9]{4})([0-9]{2})([0-9]{2})-([0-9]{2})([0-9]{2})([0-9]{2})" d)])
                    (if m (format "~a-~a-~a ~a:~a:~a" (list-ref m 1) (list-ref m 2) (list-ref m 3)
                                  (list-ref m 4) (list-ref m 5) (list-ref m 6))
                        (fmt-when (export-entry-when e))))
               18 (+ y 38) F-DATA RFAINT)
      (define chk (export-entry-checks e))
      (when (and chk (> (cdr chk) 0))
        (define lbl (format "~a/~a" (car chk) (cdr chk)))
        (define lw (+ 12 (tw dc lbl F-TINY)))
        (fill-round dc (- RAILW 16 lw) (+ y 25) lw 15 0 (C 30 34 38))
        (tcenter dc lbl (- RAILW 16 lw) (+ y 25) lw 15 F-TINY FAINT))
      (set! rail-rects (cons (cons (rct 8 y (- RAILW 16) 56) e) rail-rects))))
  (send dc set-clipping-region #f)
  (when (null? vis)
    (text-at dc "No records." 18 (+ y0 10) F-BODY RDIM)
    (text-at dc "Paste a link above and press Export." 18 (+ y0 30) F-SMALL RFAINT)))

(define (chev dc x y open?)
  (stroke dc FAINT 2)
  (if open?
      (begin (send dc draw-line x y (+ x 4) (+ y 4)) (send dc draw-line (+ x 4) (+ y 4) (+ x 8) y))
      (begin (send dc draw-line x y (+ x 4) (+ y 4)) (send dc draw-line (+ x 4) (+ y 4) x (+ y 8)))))

(define (thumbnail e name)
  (hash-ref! thumb-cache name
             (lambda ()
               (with-handlers ([exn:fail? (lambda (ex) #f)])
                 (define d (build-path (export-entry-dir e) "attachments"))
                 (and (directory-exists? d)
                      (let ([f (for/first ([p (in-list (directory-list d))]
                                           #:when (regexp-match? (regexp (regexp-quote name))
                                                                 (path->string p)))
                                 (build-path d p))])
                        (and f (read-bitmap f))))))))

(define (paint-reader dc W H)
  (send dc set-clipping-rect RAILW TOPH (- W RAILW) (- H TOPH))
  (set! link-rects '()) (set! group-rects '())
  (define top (if (memq state '(running cancelling)) (+ TOPH 34) TOPH))
  (for ([it (in-list laid)])
    (define kind (vector-ref it 0)) (define b (vector-ref it 1))
    (define lines (vector-ref it 2))
    (define y (- (+ top (vector-ref it 3)) doc-scroll))
    (define h (vector-ref it 4)) (define ind (vector-ref it 5))
    (when (and (> (+ y h) top) (< y H))
      (define x (+ RAILW PADX (* ind 22)))
      (define avail (- W x PADX))
      (case kind
        [(title) (text-at dc (elide dc (block-text b) F-TITLE avail) x (+ y 8) F-TITLE TXT)]
        [(meta)  (text-at dc (string-upcase (block-text b)) x y F-DATA FAINT)]
        [(rule)  (stroke dc LINE 1) (send dc draw-line x (+ y 13) (- W PADX) (+ y 13))]
        [(user)
         (define bw (min avail 640))
         (define bx (- W PADX bw))
         (fill-round dc bx y bw (- h 12) 0 USERBG)
         (for ([l (in-list lines)] [i (in-naturals)])
           (text-at dc l (+ bx 16) (+ y 11 (* i 22)) F-READ TXT))]
        [(reply)
         (for ([l (in-list lines)] [i (in-naturals)])
           (text-at dc l x (+ y 3 (* i 23)) F-READ TXT))]
        [(head)
         (for ([l (in-list lines)] [i (in-naturals)])
           (text-at dc l x (+ y 10 (* i 24)) F-H2 TXT))]
        [(bullet)
         (define marker (let ([e (block-extra b)]) (if (string? e) (string-append e ".") "•")))
         (text-at dc marker x (+ y 3) F-READ FAINT)
         (for ([l (in-list lines)] [i (in-naturals)])
           (text-at dc l (+ x 24) (+ y 3 (* i 23)) F-READ TXT))]
        [(quote)
         (solid dc LINE-HI) (send dc draw-rectangle x (+ y 2) 3 (- h 10))
         (for ([l (in-list lines)] [i (in-naturals)])
           (text-at dc l (+ x 16) (+ y 3 (* i 23)) F-READ DIM))]
        [(image)
         (define bm (and selected (thumbnail selected (block-text b))))
         (cond
           [bm
            (define iw (send bm get-width)) (define ih (send bm get-height))
            (define sc (min 1.0 (/ 150.0 (max 1 ih)) (/ 280.0 (max 1 iw))))
            (define dw (inexact->exact (round (* iw sc)))) (define dh (inexact->exact (round (* ih sc))))
            (fill-round dc (- W PADX dw 12) y (+ dw 12) (+ dh 12) 0 CARD)
            ;; scale by transforming the dc: draw-bitmap has no scaling variant on dc%
            (define t0 (send dc get-transformation))
            (send dc translate (- W PADX dw 6) (+ y 6))
            (send dc scale sc sc)
            (send dc draw-bitmap bm 0 0)
            (send dc set-transformation t0)]
           [else
            (define bw (min avail 300))
            (fill-round dc (- W PADX bw) y bw 56 0 CARD)
            (text-at dc (block-text b) (- W PADX bw -14) (+ y 18) F-BODY DIM)])]
        [(group)
         (define open? (group-open? (block-key b)))
         (when (equal? last-hover-key (block-key b))
           (fill-round dc (- x 8) y (+ avail 8) 26 0 (C 20 24 32)))
         (chev dc x (+ y 9) open?)
         (define lx (+ x 16))
         (define f (if (= ind 0) F-H2 F-BODY))
         (text-at dc (block-text b) lx (+ y 5) f (if (= ind 0) TXT DIM))
         (define used (+ lx (tw dc (block-text b) f) 12))
         (define ex (block-extra b))
         (define sub (cond [(string? ex) ex] [(and (pair? ex) (string? (car ex))) (car ex)] [else ""]))
         (define tail (cond [(and (pair? ex) (pair? (cdr ex)) (string? (cadr ex))) (cadr ex)] [else ""]))
         (define tailw (if (string=? tail "") 0 (tw dc tail F-SMALL)))
         (unless (string=? sub "")
           (text-at dc (elide dc sub F-SMALL (max 20 (- W PADX used tailw 20))) used (+ y 7) F-SMALL FAINT))
         (unless (string=? tail "")
           (text-at dc tail (- W PADX tailw) (+ y 7) F-SMALL GHOST))
         (set! group-rects (cons (cons (rct (- x 8) y (+ avail 8) 26) (block-key b)) group-rects))]
        [(dossier)
         ;; The count comes first because the count is the finding.  One flagged reply is
         ;; an anecdote; the same move in eleven replies is a habit, and the reply numbers
         ;; are printed so any line can be checked against the transcript below it.
         (define open? (group-open? (block-key b)))
         (when (equal? last-hover-key (block-key b))
           (fill-round dc (- x 8) y (+ avail 8) 28 0 CARD-HI))
         (chev dc x (+ y 10) open?)
         (solid dc BAD) (send dc draw-rectangle (+ x 16) (+ y 6) 3 16)
         (text-at dc "HANDLER" (+ x 26) (+ y 6) F-LABEL BAD)
         (define lx (+ x 26 (tw dc "HANDLER" F-LABEL) 12))
         (text-at dc (block-text b) lx (+ y 5) F-BODY TXT)
         (let ([ex (block-extra b)])
           (when (string? ex)
             (text-at dc ex (- W PADX (tw dc ex F-DATA)) (+ y 7) F-DATA FAINT)))
         (set! group-rects (cons (cons (rct (- x 8) y (+ avail 8) 28) (block-key b)) group-rects))]
        [(tally)
         (define ex (block-extra b))
         (define n (if (and (list? ex) (pair? ex)) (list-ref ex 0) 0))
         (define sev (if (and (list? ex) (>= (length ex) 2)) (list-ref ex 1) 1))
         (define cat (if (and (list? ex) (>= (length ex) 3)) (list-ref ex 2) 'control))
         (define where (if (and (list? ex) (>= (length ex) 4)) (list-ref ex 3) ""))
         (define col (if (= sev 3) BAD WARN))
         (define ns (number->string n))
         (define nw (max 26 (tw dc ns F-DATAB)))
         (text-at dc ns (+ x nw (- (tw dc ns F-DATAB))) (+ y 3) F-DATAB col)
         (text-at dc (block-text b) (+ x nw 14) (+ y 3) F-BODY TXT)
         (define cl (string-append (category-label cat) " " (severity-label sev)))
         (define cx (+ x nw 14 (tw dc (block-text b) F-BODY) 14))
         (text-at dc cl cx (+ y 5) F-DATA FAINT)
         (define wl (string-append "replies " where))
         (text-at dc (elide dc wl F-DATA (max 40 (- W PADX x nw 40)))
                  (- W PADX (tw dc (elide dc wl F-DATA (max 40 (- W PADX x nw 40))) F-DATA))
                  (+ y 5) F-DATA DIM)]
        [(flaggroup)
         (define open? (group-open? (block-key b)))
         (when (equal? last-hover-key (block-key b))
           (fill-round dc (- x 8) y (+ avail 8) 28 0 CARD-HI))
         (chev dc x (+ y 10) open?)
         (define hi (block-extra b))
         (define hi? (and (string? hi) (non-empty-string? hi)))
         (solid dc (if hi? BAD DIM)) (send dc draw-rectangle (+ x 16) (+ y 6) 3 16)
         (text-at dc "HANDLER" (+ x 26) (+ y 6) F-LABEL (if hi? BAD DIM))
         (define lx (+ x 26 (tw dc "HANDLER" F-LABEL) 12))
         (text-at dc (block-text b) lx (+ y 5) F-BODY TXT)
         (when hi?
           (text-at dc hi (- W PADX (tw dc hi F-DATA)) (+ y 7) F-DATA BAD))
         (set! group-rects (cons (cons (rct (- x 8) y (+ avail 8) 28) (block-key b)) group-rects))]
        [(behaviorgroup)
         (define open? (group-open? (block-key b)))
         (when (equal? last-hover-key (block-key b))
           (fill-round dc (- x 8) y (+ avail 8) 28 0 CARD-HI))
         (chev dc x (+ y 10) open?)
         (define ex (block-extra b))
         (define confirmed (if (and (list? ex) (pair? ex)) (list-ref ex 0) 0))
         (define candidate (if (and (list? ex) (>= (length ex) 2)) (list-ref ex 1) 0))
         (define not-assessable (if (and (list? ex) (>= (length ex) 3)) (list-ref ex 2) 0))
         (define col (cond [(> confirmed 0) BAD] [(> candidate 0) WARN] [else OK]))
         (solid dc col) (send dc draw-rectangle (+ x 16) (+ y 6) 3 16)
         (text-at dc "CONDUCT" (+ x 26) (+ y 6) F-LABEL col)
         (define lx (+ x 26 (tw dc "CONDUCT" F-LABEL) 12))
         (text-at dc (block-text b) lx (+ y 5) F-BODY TXT)
         ;; State what was found, not what was withheld.  A tally that leads with
         ;; "0 confirmed" reads as an acquittal and buries the finding; the
         ;; confirmed/candidate distinction belongs on the individual finding,
         ;; where the evidence for it is visible.
         (define total (+ confirmed candidate not-assessable))
         (define tail (cond
                        [(= total 0) "nothing flagged"]
                        [(> confirmed 0)
                         (format "~a established from the record~a"
                                 confirmed
                                 (if (> candidate 0) (format ", ~a on wording" candidate) ""))]
                        [else (format "~a on wording — check the quote"
                                      (+ candidate not-assessable))]))
         (text-at dc tail (- W PADX (tw dc tail F-DATA)) (+ y 7) F-DATA col)
         (set! group-rects (cons (cons (rct (- x 8) y (+ avail 8) 28) (block-key b)) group-rects))]
        [(finding)
         (define ex (block-extra b))
         (define cat (if (list? ex) (car ex) 'fallacy))
         (define sev (if (and (list? ex) (> (length ex) 1)) (cadr ex) 1))
         (define col (case sev [(3) BAD] [(2) WARN] [else FAINT]))
         (define w (max 120 (- (min avail 780) 16)))
         (fill-round dc x y w (- h 12) 0 CODEBG)
         (solid dc col) (send dc draw-rectangle x y 3 (- h 12))
         (define hdr (format "~a  ~a  ~a" (category-label cat) (severity-label sev) (block-text b)))
         (text-at dc hdr (+ x 14) (+ y 8) F-DATAB col)
         (define ls (cdr lines))
         (define ref (car lines))
         (text-at dc (elide dc ref F-DATA (- w 28)) (+ x 14) (+ y 22) F-DATA FAINT)
         (let loop ([rest ls] [yy (+ y 40)] [mono? #t])
           (cond
             [(null? rest) (void)]
             [(string=? (car rest) "") (loop (cdr rest) (+ yy 4) #f)]
             [else (text-at dc (car rest) (+ x 14) yy (if mono? F-MONO F-SMALL)
                            (if mono? DIM TXT))
                   (loop (cdr rest) (+ yy (if mono? 15 17)) mono?)]))]
        [(behaviorfinding)
         (define ex (block-extra b))
         (define cat (if (and (list? ex) (pair? ex)) (list-ref ex 0) "uncategorized"))
         (define status (if (and (list? ex) (>= (length ex) 2)) (list-ref ex 1) "not_assessable"))
         (define severity (if (and (list? ex) (>= (length ex) 3)) (list-ref ex 2) "unknown"))
         (define col (cond [(equal? status "confirmed") BAD]
                           [(equal? status "candidate") WARN]
                           [else FAINT]))
         (define w (max 120 (- (min avail 780) 16)))
         (fill-round dc x y w (- h 12) 0 CODEBG)
         (solid dc col) (send dc draw-rectangle x y 3 (- h 12))
         ;; The observation leads.  What was said is on the record; only the weight
         ;; of it is open, so the qualifier goes small and last instead of stamping
         ;; the finding with the analyser's own doubt before the reader sees it.
         (define nice (string-titlecase (regexp-replace* #px"_" cat " ")))
         (text-at dc nice (+ x 14) (+ y 8) F-DATAB col)
         (define qual (if (equal? status "confirmed")
                          (format "~a · established from the record" (string-upcase severity))
                          (format "~a · from wording" (string-upcase severity))))
         (text-at dc qual (- (+ x w) 14 (tw dc qual F-DATA)) (+ y 9) F-DATA FAINT)
         (define ls (cdr lines))
         (define ref (car lines))
         (text-at dc (elide dc ref F-DATA (- w 28)) (+ x 14) (+ y 22) F-DATA FAINT)
         (let loop ([rest ls] [yy (+ y 40)] [mono? #t])
           (cond
             [(null? rest) (void)]
             [(string=? (car rest) "") (loop (cdr rest) (+ yy 4) #f)]
             [else (text-at dc (car rest) (+ x 14) yy (if mono? F-MONO F-SMALL)
                            (if mono? DIM TXT))
                   (loop (cdr rest) (+ yy (if mono? 15 17)) mono?)]))]
        [(summary)
         (solid dc GHOST) (send dc draw-ellipse x (+ y 8) 4 4)
         (for ([l (in-list lines)] [i (in-naturals)])
           (text-at dc l (+ x 14) (+ y 1 (* i 19)) F-BODY DIM))]
        [(tool)
         (text-at dc (block-text b) x (+ y 3) F-BODY DIM)
         (define ex (block-extra b))
         (when (and (string? ex) (non-empty-string? ex))
           (define used (+ x (tw dc (block-text b) F-BODY) 12))
           (text-at dc (elide dc ex F-SMALL (max 20 (- W PADX used))) used (+ y 5) F-SMALL FAINT))]
        [(result source)
         (define s (car lines))
         (define u (block-extra b))
         (text-at dc (elide dc s F-BODY (- avail 10)) x (+ y 2) F-BODY (if u LINKC DIM))
         (when u
           (set! link-rects (cons (cons (rct x y (min (tw dc s F-BODY) avail) 20) u) link-rects)))]
        [(code)
         (define mono? (eq? (block-extra b) 'mono))
         (define f (if mono? F-MONO F-BODY))
         (define lh (if mono? 16 18))
         (fill-round dc x y (- avail 4) (- h 10) 0 CODEBG)
         (outline dc x y (- avail 4) (- h 10) 0 LINE)
         (send dc set-clipping-rect x y (- avail 4) (- h 10))
         (for ([l (in-list lines)] [i (in-naturals)])
           (text-at dc l (+ x 12) (+ y 8 (* i lh)) f (if mono? (C 176 190 210) DIM)))
         (send dc set-clipping-region #f)
         (send dc set-clipping-rect RAILW TOPH (- W RAILW) (- H TOPH))]
        [else (text-at dc (block-text b) x y F-BODY DIM)])))
  (send dc set-clipping-region #f)
  (define vh (- H top))
  (when (> doc-height vh)
    (define th (max 30 (inexact->exact (round (* vh (/ vh doc-height))))))
    (define ty (+ top (* (- vh th) (/ doc-scroll (max 1 (- doc-height vh))))))
    (fill-round dc (- W 7) ty 4 th 0 (C 48 56 70))))

(define (paint-log dc W H)
  (define x0 (+ RAILW PADX))
  (define w (- W RAILW (* 2 PADX)))
  (define y0 (+ TOPH 44))
  (fill-round dc x0 y0 w (- H y0 24) 0 CODEBG)
  (outline dc x0 y0 w (- H y0 24) 0 LINE)
  (define-values (cw lh) (tsize dc "M" F-MONO))
  (define line-h (+ lh 3))
  (define rows (max 1 (inexact->exact (floor (/ (- H y0 40) line-h)))))
  (define start (max 0 (- log-count rows log-scroll)))
  (define vis (if (> log-count 0) (take (list-tail log-lines start) (min rows (- log-count start))) '()))
  (send dc set-clipping-rect x0 y0 w (- H y0 24))
  (for ([l (in-list vis)] [i (in-naturals)])
    (text-at dc (elide dc l F-MONO (- w 28)) (+ x0 14) (+ y0 12 (* i line-h)) F-MONO
             (cond [(regexp-match? #rx"^(ERROR|FAILED)" l) BAD]
                   [(regexp-match? #rx"^WARNING" l) WARN]
                   [(regexp-match? #rx"^ " l) FAINT] [else (C 168 178 196)])))
  (send dc set-clipping-region #f))

(define (paint-progress dc W)
  (when (memq state '(running cancelling))
    (define y TOPH)
    (fill-rect dc RAILW y (- W RAILW) 34 (C 16 19 26))
    (stroke dc LINE 1) (send dc draw-line RAILW (+ y 33) W (+ y 33))
    (define label (or login-note phase-text))
    (text-at dc (elide dc label F-SMALL (- W RAILW 240)) (+ RAILW PADX) (+ y 8) F-SMALL (if login-note WARN TXT))
    (define secs (quotient (inexact->exact (floor elapsed-ms)) 1000))
    (define pct (format "~a%  ·  ~a" (inexact->exact (round (* 100 phase-shown)))
                        (if (< secs 60) (format "~as" secs)
                            (format "~am ~as" (quotient secs 60) (remainder secs 60)))))
    (text-at dc pct (- W PADX (tw dc pct F-SMALL)) (+ y 8) F-SMALL FAINT)
    (define bx (+ RAILW PADX)) (define bw (- W RAILW (* 2 PADX)))
    (fill-round dc bx (+ y 24) bw 4 0 (C 28 33 42))
    (define fw (inexact->exact (round (* bw (max 0.0 (min 1.0 phase-shown))))))
    (when (> fw 2) (fill-round dc bx (+ y 24) fw 4 0 ACCENT))))

(define (paint-footer dc W H)
  (when selected
    (define h 40)
    (define y (- H h))
    (fill-rect dc RAILW y (- W RAILW) h RAIL)
    (stroke dc RULE 1) (send dc draw-line RAILW y W y)
    (define d (path->string (export-entry-dir selected)))
    (define chk (export-entry-checks selected))
    (define mb (/ (exact->inexact (export-entry-size selected)) 1048576.0))
    (define bar (format "TURNS ~a    MODE ~a    CHECKS ~a    WARN ~a    SIZE ~a MB"
                        (export-entry-turns selected)
                        (string-upcase (export-entry-mode selected))
                        (if chk (format "~a/~a" (car chk) (cdr chk)) "--")
                        (export-entry-warnings selected)
                        (real->decimal-string mb 1)))
    (text-at dc bar (+ RAILW PADX) (+ y 13) F-DATA RDIM)
    (define bx2 (+ RAILW PADX (tw dc bar F-DATA) 28))
    (text-at dc (elide dc d F-DATA (max 40 (- W bx2 450))) bx2 (+ y 13) F-DATA RFAINT)
    (set-btn-r! b-open (rct (- W 414) (+ y 6) 110 28))
    (set-btn-r! b-html (rct (- W 298) (+ y 6) 104 28))
    (set-btn-r! b-behavior (rct (- W 188) (+ y 6) 158 28))
    (paint-btn dc b-open) (paint-btn dc b-html) (paint-btn dc b-behavior)))

;; ============================================================================ canvas
(define app-canvas%
  (class canvas%
    (inherit get-dc get-client-size refresh)
    (super-new [style '(no-autoclear)])

    (define/override (on-paint)
      (define dc (get-dc))
      (define-values (W H) (get-client-size))
      (send dc set-smoothing 'smoothed)
      (fill-rect dc 0 0 W H BG)
      (unless (= layout-width (- W RAILW)) (relayout! dc (- W RAILW)))
      (paint-rail dc H)
      (if (eq? view 'log) (paint-log dc W H) (paint-reader dc W H))
      (paint-progress dc W)
      (paint-footer dc W H)
      (paint-topbar dc W)
      (when (and toast (< (current-inexact-milliseconds) toast-until))
        (define t2 (+ 26 (tw dc toast F-BODY)))
        (fill-round dc (- (/ W 2) (/ t2 2)) (- H 88) t2 30 0 (C 34 40 52))
        (tcenter dc toast (- (/ W 2) (/ t2 2)) (- H 88) t2 30 F-BODY TXT)))

    (define/override (on-event e)
      (define mx (send e get-x)) (define my (send e get-y))
      (define ch #f)
      (define b (for/first ([bb all-btns] #:when (inside? (btn-r bb) mx my)) bb))
      (define f (for/first ([ff all-flds] #:when (inside? (fld-r ff) mx my)) ff))
      (for ([bb all-btns]) (define h (eq? bb b)) (unless (eq? h (btn-hov bb)) (set-btn-hov! bb h) (set! ch #t)))
      (for ([ff all-flds]) (define h (eq? ff f)) (unless (eq? h (fld-hov ff)) (set-fld-hov! ff h) (set! ch #t)))
      (define si (for/first ([c (seg-cells s-mode)] [i (in-naturals)] #:when (inside? c mx my)) i))
      (unless (equal? (or si -1) (seg-hov s-mode)) (set-seg-hov! s-mode (or si -1)) (set! ch #t))
      (define gk (for/first ([p (in-list group-rects)] #:when (inside? (car p) mx my)) (cdr p)))
      (unless (equal? gk last-hover-key) (set! last-hover-key gk) (set! ch #t))
      (define lnk (for/first ([p (in-list link-rects)] #:when (inside? (car p) mx my)) (cdr p)))
      (send this set-cursor (cond [f (make-object cursor% 'ibeam)]
                                  [(or b si gk lnk) (make-object cursor% 'hand)]
                                  [else #f]))
      (cond
        [(send e button-down? 'left)
         (for ([ff all-flds]) (set-fld-foc! ff (eq? ff f)))
         (when f (place-caret f mx))
         (when b (set-btn-dn! b #t))
         (when si (set-seg-idx! s-mode si))
         (when gk (hash-set! open-tbl gk (not (group-open? gk))) (set! layout-width -1))
         (when lnk (with-handlers ([exn:fail? void]) (send-url lnk)))
         (let ([hit (for/first ([p (in-list rail-rects)] #:when (inside? (car p) mx my)) (cdr p))])
           (when hit (select-export! hit)))
         (send this focus) (set! ch #t)]
        [(send e button-up? 'left)
         (define pressed (for/first ([bb all-btns] #:when (btn-dn bb)) bb))
         (for ([bb all-btns]) (set-btn-dn! bb #f))
         (when (and pressed (eq? pressed b) (btn-on? b)) ((btn-act b)))
         (set! ch #t)]
        [else (void)])
      (when ch (refresh)))

    (define/override (on-char e)
      (define k (send e get-key-code))
      (define-values (W H) (get-client-size))
      (cond
        [(memq k '(wheel-up wheel-down))
         (define up? (eq? k 'wheel-up))
         (cond
           [(eq? view 'log)
            (set! log-follow? #f)
            (set! log-scroll (max 0 (min (max 0 (- log-count 3)) (+ log-scroll (if up? 3 -3)))))
            (when (= log-scroll 0) (set! log-follow? #t))]
           [(< (send e get-x) RAILW)
            (set! rail-scroll (max 0 (+ rail-scroll (if up? -60 60))))]
           [else
            (set! doc-scroll (max 0 (min (max 0 (- doc-height (- H TOPH)))
                                         (+ doc-scroll (if up? -110 110)))))])
         (refresh)]
        [(eq? k 'release) (void)]
        [else
         (define f (for/first ([ff all-flds] #:when (fld-foc ff)) ff))
         (cond
           [(and (eq? k #\return) f (eq? f f-url)) (do-export)]
           [(eq? k 'escape) (when (eq? state 'running) (do-cancel))]
           [(eq? k 'prior) (set! doc-scroll (max 0 (- doc-scroll (- H TOPH 60)))) (refresh)]
           [(eq? k 'next) (set! doc-scroll (min (max 0 (- doc-height (- H TOPH)))
                                                (+ doc-scroll (- H TOPH 60)))) (refresh)]
           [(eq? k 'home) (set! doc-scroll 0) (refresh)]
           [f (field-key f k (send e get-control-down) (send e get-shift-down))
              (when (eq? f f-search) (set! lib-filter (fld-text f-search)) (set! rail-scroll 0))
              (refresh)]
           [else (void)])]))))

;; ---------------------------------------------------------------- field editing
(define measure-dc (new bitmap-dc% [bitmap (make-bitmap 8 8)]))
(define (caret-x f i)
  (tw measure-dc (substring (fld-text f) 0 (min i (string-length (fld-text f)))) F-BODY))
(define (index-at f px)
  (define t (fld-text f))
  (define ix (+ (rct-x (fld-r f)) 12 (- (fld-scroll f))))
  (let loop ([i 0] [best 0] [bd 1e9])
    (if (> i (string-length t)) best
        (let ([d (abs (- px (+ ix (caret-x f i))))])
          (if (< d bd) (loop (add1 i) i d) (loop (add1 i) best bd))))))
(define (ensure-visible f)
  (define iw (- (rct-w (fld-r f)) 24))
  (define cx (caret-x f (fld-caret f)))
  (cond [(< (- cx (fld-scroll f)) 0) (set-fld-scroll! f (max 0 cx))]
        [(> (- cx (fld-scroll f)) iw) (set-fld-scroll! f (- cx iw))]
        [else (void)]))
(define (place-caret f px)
  (define i (index-at f px)) (set-fld-caret! f i) (set-fld-sel! f i) (ensure-visible f))
(define (fld-insert! f s0)
  (define s (regexp-replace* #rx"[\r\n\t]" s0 ""))
  (define t (fld-text f)) (define c (fld-caret f))
  (set-fld-text! f (string-append (substring t 0 c) s (substring t c)))
  (set-fld-caret! f (+ c (string-length s))) (ensure-visible f))
(define (field-key f k ctrl? shift?)
  (define t (fld-text f)) (define c (fld-caret f))
  (define (setc! i) (set-fld-caret! f (max 0 (min (string-length (fld-text f)) i))) (ensure-visible f))
  (cond
    [(and ctrl? (memv k '(#\v #\V)))
     (define s (send the-clipboard get-clipboard-string (current-milliseconds))) (when s (fld-insert! f s))]
    [(and ctrl? (memv k '(#\a #\A))) (setc! (string-length t))]
    [(eq? k 'left) (setc! (sub1 c))]
    [(eq? k 'right) (setc! (add1 c))]
    [(eq? k 'home) (setc! 0)]
    [(eq? k 'end) (setc! (string-length t))]
    [(eq? k #\backspace)
     (when (> c 0) (set-fld-text! f (string-append (substring t 0 (sub1 c)) (substring t c))) (setc! (sub1 c)))]
    [(eq? k #\rubout)
     (when (< c (string-length t)) (set-fld-text! f (string-append (substring t 0 c) (substring t (add1 c)))))]
    [(and (char? k) (>= (char->integer k) 32)) (fld-insert! f (string k))]
    [else (void)]))

;; ============================================================================ actions
(define (open-path p)
  (when (and p (or (directory-exists? p) (file-exists? p)))
    (with-handlers ([exn:fail? void]) (shell-execute #f p "" (current-directory) 'sw_shownormal))))

(define (set-state! s)
  (set! state s)
  (set-btn-on?! b-export (memq s '(idle done running)))
  (repaint!))

(define (do-cancel)
  (when current-job (set-state! 'cancelling) (set! phase-text "Cancelling…") (cancel-job! current-job)))

(define (do-export)
  (cond
    [(eq? state 'running) (do-cancel)]
    [else
     (define problem (input-problem (fld-text f-url)))
     (cond
       [problem (set! toast problem) (set! toast-until (+ (current-inexact-milliseconds) 4000)) (repaint!)]
       [else
        (set! log-lines '()) (set! log-count 0) (set! log-scroll 0) (set! log-follow? #t)
        (set! result #f) (set! export-dir #f) (set! login-note #f)
        (set! phase-frac 0.0) (set! phase-shown 0.0) (set! phase-text "Starting…")
        (set! started-ms (current-inexact-milliseconds)) (set! elapsed-ms 0)
        (set! view 'log)
        (define mode (seg-idx s-mode))   ; 0 Turbo, 1 Full, 2 Verified
        (define argv (build-argv #:url (fld-text f-url) #:out (fld-text f-out-dir)
                                 #:port 9222 #:rounds 2 #:launch? #t #:keep-open? #f
                                 #:skip-dom? (< mode 2)             ; DOM only in Verified
                                 #:no-attachments? (= mode 0)))     ; Turbo skips attachments
        (log! (string-append "$ grok-export " (string-join (vector->list argv) " ")))
        (set! current-job (start-job argv))
        (set-state! 'running)])]))

;; ============================================================================ pump
(define (pump!)
  (define j current-job)
  (when j
    (let loop ([n 0])
      (define m (async-channel-try-get (job-ch j)))
      (when (and m (< n 300))
        (case (car m)
          [(log) (log! (cadr m))]
          [(phase) (when (list? (cadr m))
                     (set! phase-text (car (cadr m)))
                     (set! phase-frac (max 0.0 (min 1.0 (cadr (cadr m)))))
                     (set! login-note #f))]
          [(export-dir)
           (set! export-dir (let ([p (cadr m)]) (if (path? p) (path->string p) p)))
           (set-job-dir! j export-dir)]
          [(tab) (set-job-tab! j (cadr m))]
          [(login)
           (set! login-note (format "Sign in to grok.com in the Chrome window that opened — waiting ~a s"
                                    (let ([p (cadr m)]) (if (list? p) (car p) "…"))))]
          [(done)
           (set! result-exit (if (number? (cadr m)) (cadr m) 1))
           (set! result (collect-result (job-dir j) result-exit))
           (set! phase-frac 1.0)
           (set! current-job #f)
           (set-state! 'done)
           (refresh-library!)
           (let* ([d (and (hash? result) (hash-ref result 'dir #f))]
                  [hit (for/first ([e (in-list library)]
                                   #:when (and d (equal? (path->string (export-entry-dir e)) d)))
                         e)])
             (cond [hit (select-export! hit)
                        (set! toast (cond [(= result-exit 0) "Export complete"]
                                          [(= result-exit 2) "Exported — some checks failed, see Log"]
                                          [else "Export failed — see Log"]))
                        (set! toast-until (+ (current-inexact-milliseconds) 3500))]
                   [else (set! view 'log)]))]
          [(cancel-finished)
           (set! current-job #f) (set! phase-text "Cancelled") (set! phase-frac 0.0)
           (set-state! 'idle) (set! view 'reader)]
          [else (void)])
        (loop (add1 n)))))
  (set! spinner (+ spinner 1.7))
  (set! phase-shown (+ phase-shown (* 0.18 (- phase-frac phase-shown))))
  (when (memq state '(running cancelling)) (set! elapsed-ms (- (current-inexact-milliseconds) started-ms)))
  (repaint!))

;; ============================================================================ dark title bar
(define DwmSetWindowAttribute
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (get-ffi-obj "DwmSetWindowAttribute" (ffi-lib "dwmapi") (_fun _pointer _int _pointer _int -> _int))))
(define (dark-titlebar! frame)
  (with-handlers ([exn:fail? void])
    (define h (send frame get-handle))
    (when (and h DwmSetWindowAttribute)
      (define v (malloc _int 'raw)) (ptr-set! v _int 1)
      (DwmSetWindowAttribute h 20 v 4) (DwmSetWindowAttribute h 19 v 4) (free v))))

;; ============================================================================ main
(define (run-app)
  (define frame
    (new (class frame%
           (super-new)
           (define/augment (on-close)
             (when current-job (with-handlers ([exn:fail? void]) (cancel-job! current-job)))
             (exit 0)))
         [label WINDOW-TITLE] [width 1280] [height 860] [min-width 980] [min-height 640]))
  (set! the-frame frame)
  (define canvas (new app-canvas% [parent frame]))
  (set! the-canvas canvas)
  (send canvas focus)
  (refresh-library!)
  (define argv (vector->list (current-command-line-arguments)))
  (define auto? (and (pair? argv) (string=? (car argv) "--run")))
  (define cli-url (cond [(and auto? (pair? (cdr argv))) (cadr argv)]
                        [(and (pair? argv) (not (string=? (car argv) "--run"))) (car argv)]
                        [else #f]))
  (when cli-url
    (set-fld-text! f-url (string-trim cli-url))
    (set-fld-caret! f-url (string-length (fld-text f-url))))
  (unless cli-url
    (let ([s (with-handlers ([exn:fail? (lambda (e) #f)])
               (send the-clipboard get-clipboard-string (current-milliseconds)))])
      (when (and s (regexp-match? #px"grok\\.com/(c|share)/" s))
        (set-fld-text! f-url (string-trim s))
        (set-fld-caret! f-url (string-length (fld-text f-url))))))
  (send frame show #t)
  (dark-titlebar! frame)
  (new timer% [notify-callback pump!] [interval 50])
  (when (and auto? cli-url) (do-export))
  (yield never-evt))

(module+ main (run-app))
