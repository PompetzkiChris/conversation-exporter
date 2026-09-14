#lang racket/base
;; ws.rkt — hand-rolled RFC 6455 WebSocket client over racket/tcp.
;;  * client frames are masked; 7/16/64-bit payload lengths on both directions
;;  * continuation frames are reassembled; ping -> pong answered inline; close handled
;;  * Sec-WebSocket-Accept verified with SHA-1 (file/sha1) + base64
(require racket/tcp
         racket/string
         racket/random
         file/sha1
         net/base64
         "http.rkt")

(provide ws-connect
         ws-send-text
         ws-send-frame
         ws-recv
         ws-close
         ws-closed?
         ws-stats
         ws?)

(define WS-GUID "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

(struct ws (in out send-sema [closed? #:mutable] stats-field))
;; stats vector: 0 frames with 7-bit length, 1 with 16-bit, 2 with 64-bit, 3 continuation
;; frames, 4 pings answered, 5 messages delivered, 6 largest message bytes, 7 frames sent,
;; 8 largest frame sent
(define (ws-stats w)
  (define v (ws-stats-vec w))
  (hasheq 'framesReceived7bit (vector-ref v 0)
          'framesReceived16bit (vector-ref v 1)
          'framesReceived64bit (vector-ref v 2)
          'continuationFrames (vector-ref v 3)
          'pingsAnswered (vector-ref v 4)
          'messagesReceived (vector-ref v 5)
          'largestMessageBytes (vector-ref v 6)
          'framesSent (vector-ref v 7)
          'largestFrameSentBytes (vector-ref v 8)))
(define (ws-stats-vec w) (ws-stats-field w))
(define (bump! w i [n 1]) (define v (ws-stats-field w)) (vector-set! v i (+ (vector-ref v i) n)))
(define (max! w i n) (define v (ws-stats-field w)) (when (> n (vector-ref v i)) (vector-set! v i n)))

(define (ws-connect url)
  (define m (regexp-match #px"^ws://([^/:]+)(?::(\\d+))?(/.*)?$" url))
  (unless m (error 'ws-connect "unsupported WebSocket URL: ~a" url))
  (define host (cadr m))
  (define port (if (caddr m) (string->number (caddr m)) 80))
  (define path (or (cadddr m) "/"))
  (define key (bytes->string/utf-8 (base64-encode (crypto-random-bytes 16) #"")))
  (define-values (in out) (tcp-connect host port))
  (write-string (format (string-append "GET ~a HTTP/1.1\r\nHost: ~a:~a\r\n"
                                       "Upgrade: websocket\r\nConnection: Upgrade\r\n"
                                       "Sec-WebSocket-Key: ~a\r\nSec-WebSocket-Version: 13\r\n\r\n")
                        path host port key)
                out)
  (flush-output out)
  (define status-line (read-line in 'return-linefeed))
  (when (eof-object? status-line)
    (error 'ws-connect "connection closed during handshake to ~a" url))
  (unless (regexp-match? #px"^HTTP/1\\.1 101" status-line)
    (define headers (read-http-headers in))
    (error 'ws-connect "handshake to ~a rejected: ~a (headers: ~s)" url status-line headers))
  (define headers (read-http-headers in))
  (define accept (let ([h (assoc "sec-websocket-accept" headers)]) (and h (cdr h))))
  (define expected
    (bytes->string/utf-8
     (base64-encode (sha1-bytes (open-input-bytes (string->bytes/utf-8 (string-append key WS-GUID)))) #"")))
  (unless (and accept (string=? (string-trim accept) expected))
    (error 'ws-connect "Sec-WebSocket-Accept mismatch: got ~s expected ~s" accept expected))
  (ws in out (make-semaphore 1) #f (make-vector 9 0)))

(define (mask-bytes! dst src mask)
  (define n (bytes-length src))
  (let loop ([i 0])
    (when (< i n)
      (bytes-set! dst i (bitwise-xor (bytes-ref src i) (bytes-ref mask (bitwise-and i 3))))
      (loop (add1 i))))
  dst)

;; Send one complete (FIN) masked frame.
(define (ws-send-frame w opcode payload)
  (define len (bytes-length payload))
  (define mask (crypto-random-bytes 4))
  (define b0 (bitwise-ior #x80 opcode))
  (define header
    (cond
      [(< len 126) (bytes b0 (bitwise-ior #x80 len))]
      [(< len 65536) (bytes b0 (bitwise-ior #x80 126)
                            (arithmetic-shift len -8) (bitwise-and len 255))]
      [else (bytes-append (bytes b0 (bitwise-ior #x80 127))
                          (integer->integer-bytes len 8 #f #t))]))
  (define masked (mask-bytes! (make-bytes len) payload mask))
  (call-with-semaphore
   (ws-send-sema w)
   (lambda ()
     (define out (ws-out w))
     (write-bytes header out)
     (write-bytes mask out)
     (write-bytes masked out)
     (flush-output out)
     (bump! w 7)
     (max! w 8 len))))

(define (ws-send-text w str)
  (ws-send-frame w 1 (string->bytes/utf-8 str)))

(define (read-exactly in n)
  (cond
    [(zero? n) #""]
    [else
     (define b (read-bytes n in))
     (when (or (eof-object? b) (< (bytes-length b) n))
       (error 'ws "connection closed while reading a frame (wanted ~a bytes)" n))
     b]))

;; -> (values type payload) where type is 'text, 'binary or 'close.
;; Reassembles fragmented messages; answers pings; ignores pongs.
(define (ws-recv w)
  (define in (ws-in w))
  (let loop ([frags '()] [type #f])
    (define h (read-exactly in 2))
    (define b0 (bytes-ref h 0))
    (define b1 (bytes-ref h 1))
    (define fin? (bitwise-bit-set? b0 7))
    (define opcode (bitwise-and b0 15))
    (define masked? (bitwise-bit-set? b1 7))
    (define len7 (bitwise-and b1 127))
    (define len
      (cond
        [(= len7 126) (bump! w 1) (integer-bytes->integer (read-exactly in 2) #f #t)]
        [(= len7 127) (bump! w 2) (integer-bytes->integer (read-exactly in 8) #f #t)]
        [else (bump! w 0) len7]))
    (when (= opcode 0) (bump! w 3))
    (define mask (and masked? (read-exactly in 4)))
    (define raw (read-exactly in len))
    (define payload (if mask (mask-bytes! (make-bytes len) raw mask) raw))
    (case opcode
      [(9)  (unless (ws-closed? w) (ws-send-frame w 10 payload) (bump! w 4)) (loop frags type)]   ; ping -> pong
      [(10) (loop frags type)]                                                         ; pong
      [(8)  ;; server-initiated close: echo the status code back (RFC 6455 5.5.1), then mark closed
       (unless (ws-closed? w)
         (with-handlers ([exn:fail? void])
           (ws-send-frame w 8 (if (>= (bytes-length payload) 2) (subbytes payload 0 2) #"")))
         (set-ws-closed?! w #t))
       (values 'close payload)]
      [(0 1 2)
       (define type* (cond [(= opcode 1) 'text] [(= opcode 2) 'binary] [else (or type 'binary)]))
       (define frags* (cons payload frags))
       (cond
         [fin?
          (define msg (if (null? (cdr frags*)) payload (apply bytes-append (reverse frags*))))
          (bump! w 5)
          (max! w 6 (bytes-length msg))
          (values type* msg)]
         [else (loop frags* type*)])]
      [else (error 'ws "unknown opcode ~a" opcode)])))

(define (ws-close w)
  (unless (ws-closed? w)
    (set-ws-closed?! w #t)
    (with-handlers ([exn:fail? void])
      (ws-send-frame w 8 (bytes 3 232))))   ; status 1000
  (with-handlers ([exn:fail? void]) (close-output-port (ws-out w)))
  (with-handlers ([exn:fail? void]) (close-input-port (ws-in w))))
