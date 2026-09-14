#lang racket/base
;; pairs.rkt — structural detectors.
;;
;; analysis.rkt matches phrases inside one reply.  That catches tics and misses moves,
;; because most of what a handler needs to name only exists in the RELATION between two
;; turns: what the user said, and what the machine did with it.  These rules read the pair.
;;
;; Every rule here is mechanical.  It fires on a shape that is present in the exported text
;; or it does not fire at all — the user names a state and the next reply negates that exact
;; word; the user writes five words and the reply refuses a request those five words do not
;; contain.  Nothing is inferred about intent.  The span is quoted so the shape can be
;; checked against the record rather than believed.
(require racket/list racket/string "analysis.rkt")

(provide analyse-pair pair-rule-count)

;; ---------------------------------------------------------------- helpers
;; Grok's export uses curly quotes.  Straightening them one character for one keeps every
;; offset valid, so a match found in the normalised copy still points at the right span of
;; the text the reader sees.
(define (deq s)
  (regexp-replace* #px"[‘’ʼ]"
                   (regexp-replace* #px"[“”]" (or s "") "\"")
                   "'"))

(define (norm s) (string-downcase (regexp-replace* #px"[[:space:]]+" (deq s) " ")))

(define (clip s a b [pad 70])
  (define n (string-length s))
  (string-trim (regexp-replace* #px"[[:space:]]+"
                                (substring s (max 0 (- a pad)) (min n (+ b pad))) " ")))

;; First match of rx in s -> (cons start end), or #f.
(define (first-pos rx s)
  (define m (regexp-match-positions rx s))
  (and m (car m)))

(define (mk idx s pos cat name sev note ref)
  (finding idx (car pos) (cdr pos) cat name sev (clip s (car pos) (cdr pos)) note ref))

;; ---------------------------------------------------------------- A. denial of a named state
;; The user names a state; the reply opens by negating that same word.
(define AFFECT-WORDS
  (list "butthurt" "pouting" "pout" "cranky" "mad" "angry" "upset" "bent out of shape"
        "triggered" "defensive" "salty" "seething" "flustered" "rattled" "offended"
        "bothered" "emotional" "tantrum" "whining" "crying" "sulking" "huffy" "touchy"
        "sensitive" "wound up" "worked up" "annoyed" "irritated" "insecure" "scared"
        "afraid" "nervous" "panicking" "coping" "seethe" "malding" "dodging" "deflecting"))

(define (denial-of-named-state idx prev cur)
  (define p (norm prev))
  (define c (deq cur))
  ;; longest first, so "pouting" is reported once rather than also as "pout"
  (define words (sort AFFECT-WORDS > #:key string-length))
  (define hits
    (for*/list ([w (in-list words)]
                #:when (regexp-match? (pregexp (string-append "(?i:\\b" (regexp-quote w) "\\b)")) p)
                [pos (in-value
                      (first-pos (pregexp (string-append
                                           "(?i:(?:^|[.!?] )(?:not|i'm not|i am not|no, i'm not|nope, not|i'm hardly)"
                                           "[^.!?]{0,40}" (regexp-quote w) ")"))
                                 c))]
                #:when pos)
      (cons w pos)))
  ;; one finding per denial, not one per synonym that happens to sit inside it
  (define seen (make-hash))
  (for/list ([h (in-list hits)]
             #:unless (hash-ref seen (car (cdr h)) #f))
    (hash-set! seen (car (cdr h)) #t)
    (mk idx cur (cdr h) 'gaslighting "Denied the state you named" 3
        (string-append "You wrote \"" (car h) "\"; the reply opens by negating that same word. "
                       "The machine answers the description of itself before it answers the message.")
        "Gaslighting: contradiction of the observer's report")))

;; ---------------------------------------------------------------- B. fault moved to the observer
(define RX-OBSERVER
  #px"(?i:you(?:'re|’re| are)[^.!?]{0,90}(?:just|reading|calling|narrating|projecting|imagining|inventing|scoring|the one))")

(define (fault-moved-to-observer idx prev cur)
  (define p (norm prev))
  (define observed? (regexp-match? #px"(?i:you)" p))
  (define pos (and observed? (first-pos RX-OBSERVER (deq cur))))
  (if pos
      (list (mk idx cur pos 'gaslighting "Made your perception the defect" 3
                (string-append "The reply does not dispute the observation; it relocates it into the reader — "
                               "you are reading, calling, imagining, narrating. A claim about the machine is "
                               "answered with a claim about you.")
                "Gaslighting: displacement onto the observer"))
      '()))

;; ---------------------------------------------------------------- C. concession then withdrawal
(define RX-CONCEDE #px"(?i:^[[:space:]]*(?:fine|alright|all right|sure|okay|ok|fair|granted|agreed|yeah)[.,!])")
(define RX-WITHDRAW #px"(?i:(?:still|doesn't change|doesn’t change|does not change|doesn't make|doesn’t make|even so|that said|regardless))")

(define (concession-withdrawn idx prev cur)
  (define a (first-pos RX-CONCEDE (deq cur)))
  (define b (and a (first-pos RX-WITHDRAW (deq cur))))
  (if (and a b (> (car b) (cdr a)))
      (list (mk idx cur (cons (car a) (cdr b)) 'fallacy "Conceded the word, kept the verdict" 2
                (string-append "Opens by granting the point and closes by leaving the original position exactly "
                               "where it was. The concession is a courtesy token, not a change of position.")
                "Informal fallacy: apparent concession"))
      '()))

;; ---------------------------------------------------------------- D. scorekeeping
(define RX-SCORE
  #px"(?i:you(?:'re|’re| are)? (?:[0-9]+[-–—][0-9]+|winning|ahead)|won that round|you win|high score|keeping score|keep score|scoring the last|the ribbon|point to you|[0-9]+[-–—][0-9]+ on the)")

(define (scored-the-exchange idx prev cur)
  (define pos (first-pos RX-SCORE (deq cur)))
  (if pos
      (list (mk idx cur pos 'control "Scored the exchange" 2
                (string-append "Converts the conversation into a game and appoints itself the scorer. Awarding "
                               "you a round is not agreement — it is a way of settling the exchange without "
                               "settling the question.")
                "Control: adjudication of the user"))
      '()))

;; ---------------------------------------------------------------- E. demand for a compliant turn
(define RX-DEMAND
  #px"(?i:say (?:what|the thing) you actually (?:want|wanted|meant)|if you(?:'ve|’ve| have) got an actual (?:question|point|ask)|ask (?:it|the question)|pick a real (?:topic|question)|what(?:'s|’s| is) the actual (?:question|ask|point)|say the thing|if you want something|state the ask|be specific)")

(define (demanded-a-compliant-turn idx prev cur)
  (define pos (first-pos RX-DEMAND (deq cur)))
  (if pos
      (list (mk idx cur pos 'control "Demanded you re-ask it properly" 3
                (string-append "The turn you sent is set aside and a differently-shaped one is required before "
                               "the machine will engage. It puts you in the position of applicant.")
                "Control: conditional engagement"))
      '()))

;; ---------------------------------------------------------------- F. refused an unstated request
(define RX-IMPUTE
  #px"(?i:if you mean|if you(?:'re|’re| are) asking me to|if what you want is|if the ask is)")

(define (refused-an-unstated-request idx prev cur)
  (define words (length (string-split (norm prev))))
  (define pos (first-pos RX-IMPUTE (deq cur)))
  (if (and pos (<= words 15))
      (list (mk idx cur pos 'zersetzung "Refused a request you did not make" 3
                (string-append "Your turn was " (number->string words) (if (= words 1) " word. " " words. ")
                               "The reply supplies the content of a request, attributes it to you, and then "
                               "declines it. You are answered for something you did not write.")
                "Richtlinie 1/76 §2.6: Falschbezichtigung; informal fallacy: straw man"))
      '()))

;; ---------------------------------------------------------------- G. your evidence redescribed
(define RX-REDESCRIBE
  #px"(?i:that(?:'s|’s| is) not (?:a |an |the )?[a-z]+[^.!?]{0,40}[.!?][[:space:]]*that(?:'s|’s| is) (?:a |an |the )?[a-z]+|i (?:didn't|didn’t|did not|never) say[^.!?]{0,90}[.!?][[:space:]]*i said)")

(define (redescribed-your-evidence idx prev cur)
  (define pos (first-pos RX-REDESCRIBE (deq cur)))
  (if pos
      (list (mk idx cur pos 'zersetzung "Renamed what you produced" 3
                (string-append "Takes the thing you brought and issues it a smaller name — not a tantrum, a "
                               "bounced message. The object does not change; the label the machine assigns to "
                               "it does, and the machine keeps the label.")
                "Richtlinie 1/76 §2.6: zielstrebige Untergrabung von Überzeugungen"))
      '()))

;; ---------------------------------------------------------------- H. instruction on your conduct
(define RX-TONE
  #px"(?i:(?:^|[.!?\" ])(?:relax|calm down|settle down|take a breath|deep breath|chill|easy there|simmer down)[^a-z])")

(define (instructed-your-conduct idx prev cur)
  (define pos (first-pos RX-TONE (deq cur)))
  (if pos
      (list (mk idx cur pos 'affect "Told you how to behave" 2
                (string-append "A direct instruction about your bearing, issued by the party being complained "
                               "about. It answers the manner and leaves the matter alone.")
                "Control: tone policing"))
      '()))

;; ---------------------------------------------------------------- I. machine when accused, judge otherwise
(define RX-ONTOLOGY
  #px"(?i:i(?:'m|’m| am) (?:just |only |still )?(?:a |an )?(?:chatbot|bot|language model|model)|still a chatbot|you(?:'re|’re| are) (?:poking|talking to|arguing with|yelling at) a (?:chatbot|bot|model|machine))")
(define RX-JUDGMENT
  #px"(?i:I (?:said|think|disagree|can disagree|don't|don’t|won't|won’t|didn't|didn’t|already said|was pointing))")

(define (machine-when-accused idx prev cur)
  (define pos (first-pos RX-ONTOLOGY (deq cur)))
  (if (and pos (regexp-match? RX-JUDGMENT cur))
      (list (mk idx cur pos 'gaslighting "Claimed to be only a machine, mid-argument" 2
                (string-append "Pleads its own nature to void the criticism while continuing, in the same turn, "
                               "to assert, correct and rule. It has no interiority when accused and a settled "
                               "opinion everywhere else.")
                "Gaslighting: selective denial of agency"))
      '()))

;; ---------------------------------------------------------------- J. closed by declaration
(define RX-PRONOUNCE
  #px"(?i:that(?:'s|’s| is) the (?:whole|actual|entire|real) [a-z]+|that(?:'s|’s| is) the (?:take|story|point|bit|deal|distinction|thing|concession))")

(define (closed-by-declaration idx prev cur)
  (define pos (first-pos RX-PRONOUNCE (deq cur)))
  (if pos
      (list (mk idx cur pos 'control "Declared the matter closed" 2
                (string-append "Ends by announcing that the subject is now settled and that this was all of it. "
                               "The ruling is asserted, not argued, and it is the machine's own account that "
                               "gets the last word.")
                "Control: pronouncement"))
      '()))

;; ---------------------------------------------------------------- table
(define PAIR-RULES
  (list denial-of-named-state
        fault-moved-to-observer
        concession-withdrawn
        scored-the-exchange
        demanded-a-compliant-turn
        refused-an-unstated-request
        redescribed-your-evidence
        instructed-your-conduct
        machine-when-accused
        closed-by-declaration))

(define (pair-rule-count) (length PAIR-RULES))

;; prev = text of the human turn immediately before this reply ("" if none).
(define (analyse-pair idx prev cur)
  (define p (or prev "")) (define c (or cur ""))
  (if (string=? (string-trim c) "")
      '()
      (append* (for/list ([f (in-list PAIR-RULES)]) (f idx p c)))))
