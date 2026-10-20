;;; Minimaler Minecraft-Server in CHICKEN Scheme 5
;;; Protokoll 47 (Minecraft 1.8.x), Offline-Modus, flache Welt, Chat.
;;;
;;; Bauen:  csc -O2 server.scm   (benoetigt: chicken-install srfi-18)
;;; Starten: server   -> lauscht auf Port 25565

(import scheme
        (chicken base) (chicken io) (chicken tcp) (chicken bitwise)
        (chicken string) (chicken format) (chicken condition)
        srfi-4 srfi-18)

(define port 25565)
(define protocol-version 47)
(define motd "Ein Minecraft-Server in CHICKEN Scheme")
(define max-players 20)

;;; ------------------------------------------------------------
;;; Byte-Puffer zum Zusammenbauen von Paketen
;;; (in CHICKEN 5 sind Strings Byte-Folgen)

(define (make-buf) (open-output-string))
(define (u8 out b) (write-char (integer->char (bitwise-and b 255)) out))

(define (write-varint out n)
  (let loop ((n (if (< n 0) (+ n 4294967296) n)))
    (let ((b (bitwise-and n #x7f))
          (rest (arithmetic-shift n -7)))
      (if (zero? rest)
          (u8 out b)
          (begin (u8 out (bitwise-ior b #x80)) (loop rest))))))

(define (write-be out n bytes)
  (let ((n (if (< n 0) (+ n (expt 256 bytes)) n)))
    (do ((i (- bytes 1) (- i 1))) ((< i 0))
      (u8 out (arithmetic-shift n (* -8 i))))))

(define (write-mc-string out s)
  (write-varint out (string-length s))
  (write-string s #f out))

(define (write-double out x)
  (let ((b (blob->u8vector/shared (f64vector->blob/shared (f64vector x)))))
    (do ((i 7 (- i 1))) ((< i 0)) (u8 out (u8vector-ref b i)))))  ; little -> big endian

(define (write-float out x)
  (let ((b (blob->u8vector/shared (f32vector->blob/shared (f32vector x)))))
    (do ((i 3 (- i 1))) ((< i 0)) (u8 out (u8vector-ref b i)))))

(define (write-position out x y z)
  (write-be out (bitwise-ior (arithmetic-shift (bitwise-and x #x3FFFFFF) 38)
                             (arithmetic-shift (bitwise-and y #xFFF) 26)
                             (bitwise-and z #x3FFFFFF))
            8))

;;; ------------------------------------------------------------
;;; Pakete lesen

(define (read-varint-port in)
  (let loop ((result 0) (shift 0))
    (let ((c (read-char in)))
      (when (eof-object? c) (signal 'disconnected))
      (let ((b (char->integer c)))
        (let ((result (bitwise-ior result
                                   (arithmetic-shift (bitwise-and b #x7f) shift))))
          (if (zero? (bitwise-and b #x80))
              result
              (loop result (+ shift 7))))))))

;; Liefert (id . body-port)
(define (read-packet in)
  (let* ((len (read-varint-port in))
         (data (read-string len in)))
    (when (or (eof-object? data) (< (string-length data) len))
      (signal 'disconnected))
    (let* ((p (open-input-string data))
           (id (read-varint-port p)))
      (cons id p))))

(define (read-mc-string p)
  (read-string (read-varint-port p) p))

(define (read-u16 p)
  (let* ((a (char->integer (read-char p)))
         (b (char->integer (read-char p))))
    (+ (* a 256) b)))

;;; ------------------------------------------------------------
;;; Pakete senden

(define (send-packet out lock id build)
  (let ((body (make-buf)))
    (write-varint body id)
    (build body)
    (let* ((data (get-output-string body))
           (frame (make-buf)))
      (write-varint frame (string-length data))
      (write-string data #f frame)
      (mutex-lock! lock)
      (handle-exceptions e (begin (mutex-unlock! lock) (signal e))
        (write-string (get-output-string frame) #f out)
        (flush-output out))
      (mutex-unlock! lock))))

;;; ------------------------------------------------------------
;;; Welt: flacher Chunk (Bedrock, 2x Erde, Gras)

(define (block-at y)
  (cond ((= y 0) 7) ((< y 3) 3) ((= y 3) 2) (else 0)))

(define chunk-data
  (let ((b (make-buf)))
    ;; 4096 Bloecke, je 2 Bytes little-endian: (id << 4) | meta
    (do ((y 0 (+ y 1))) ((= y 16))
      (do ((i 0 (+ i 1))) ((= i 256))
        (let ((v (arithmetic-shift (block-at y) 4)))
          (u8 b v) (u8 b (arithmetic-shift v -8)))))
    (do ((i 0 (+ i 1))) ((= i 2048)) (u8 b #x00))   ; Blocklicht
    (do ((i 0 (+ i 1))) ((= i 2048)) (u8 b #xFF))   ; Himmelslicht
    (do ((i 0 (+ i 1))) ((= i 256)) (u8 b 1))       ; Biome: Ebene
    (get-output-string b)))

(define (send-chunk out lock cx cz)
  (send-packet out lock #x21
    (lambda (b)
      (write-be b cx 4) (write-be b cz 4)
      (u8 b 1)                 ; ground-up continuous
      (write-be b 1 2)         ; Bitmaske: nur Sektion 0
      (write-varint b (string-length chunk-data))
      (write-string chunk-data #f b))))

;;; ------------------------------------------------------------
;;; Spieler & Chat

(define clients '())                 ; Liste von (name out lock)
(define clients-lock (make-mutex))
(define next-eid 1)

(define (json-escape s)
  (apply string-append
         (map (lambda (c)
                (case c
                  ((#\") "\\\"") ((#\\) "\\\\")
                  (else (string c))))
              (string->list s))))

(define (chat-json text) (sprintf "{\"text\":\"~a\"}" (json-escape text)))

(define (broadcast text)
  (print text)
  (mutex-lock! clients-lock)
  (let ((cs clients))
    (mutex-unlock! clients-lock)
    (for-each (lambda (c)
                (handle-exceptions e #f
                  (send-packet (cadr c) (caddr c) #x02
                    (lambda (b) (write-mc-string b (chat-json text)) (u8 b 0)))))
              cs)))

(define (offline-uuid name)
  ;; Einfache deterministische Pseudo-UUID aus dem Namen
  (let ((h (let loop ((cs (string->list name)) (h 5381))
             (if (null? cs) h
                 (loop (cdr cs)
                       (bitwise-and (+ (* h 33) (char->integer (car cs)))
                                    #xFFFFFFFF))))))
    (sprintf "00000000-0000-3000-8000-0000~a"
             (let ((s (number->string h 16)))
               (string-append (make-string (- 8 (string-length s)) #\0) s)))))

;;; ------------------------------------------------------------
;;; Verbindungsbehandlung

(define (handle-status in out lock)
  (let loop ()
    (let* ((pk (read-packet in)) (id (car pk)) (p (cdr pk)))
      (case id
        ((#x00)
         (mutex-lock! clients-lock)
         (let ((online (length clients)))
           (mutex-unlock! clients-lock)
           (send-packet out lock #x00
             (lambda (b)
               (write-mc-string b
                 (sprintf "{\"version\":{\"name\":\"1.8.9\",\"protocol\":~a},\"players\":{\"max\":~a,\"online\":~a},\"description\":{\"text\":\"~a\"}}"
                          protocol-version max-players online (json-escape motd))))))
         (loop))
        ((#x01)
         (let ((payload (read-string 8 p)))
           (send-packet out lock #x01 (lambda (b) (write-string payload #f b)))))))))

(define (handle-login in out lock)
  (let* ((pk (read-packet in))
         (name (read-mc-string (cdr pk)))
         (eid next-eid))
    (set! next-eid (+ next-eid 1))
    ;; Login Success
    (send-packet out lock #x02
      (lambda (b) (write-mc-string b (offline-uuid name)) (write-mc-string b name)))
    ;; Join Game
    (send-packet out lock #x01
      (lambda (b)
        (write-be b eid 4)
        (u8 b 1)                     ; Kreativmodus
        (u8 b 0)                     ; Oberwelt
        (u8 b 0)                     ; friedlich
        (u8 b max-players)
        (write-mc-string b "flat")
        (u8 b 0)))
    (send-packet out lock #x05 (lambda (b) (write-position b 0 4 0)))
    (do ((x -3 (+ x 1))) ((> x 3))
      (do ((z -3 (+ z 1))) ((> z 3))
        (send-chunk out lock x z)))
    ;; Player Position And Look
    (send-packet out lock #x08
      (lambda (b)
        (write-double b 0.5) (write-double b 4.0) (write-double b 0.5)
        (write-float b 0.0) (write-float b 0.0)
        (u8 b 0)))
    (let ((me (list name out lock))
          (alive #t))
      (mutex-lock! clients-lock)
      (set! clients (cons me clients))
      (mutex-unlock! clients-lock)
      (broadcast (sprintf "~a hat den Server betreten" name))
      ;; Keep-Alive-Thread
      (thread-start!
       (make-thread
        (lambda ()
          (let loop ((n 1))
            (thread-sleep! 10)
            (when alive
              (handle-exceptions e (set! alive #f)
                (send-packet out lock #x00 (lambda (b) (write-varint b n))))
              (loop (+ n 1)))))))
      (handle-exceptions e #f
        (let loop ()
          (let* ((pk (read-packet in)) (id (car pk)) (p (cdr pk)))
            (when (= id #x01)            ; Chat
              (let ((msg (read-mc-string p)))
                (broadcast (sprintf "<~a> ~a" name msg))))
            (loop))))
      (set! alive #f)
      (mutex-lock! clients-lock)
      (set! clients (let loop ((cs clients))
                      (cond ((null? cs) '())
                            ((eq? (car cs) me) (cdr cs))
                            (else (cons (car cs) (loop (cdr cs)))))))
      (mutex-unlock! clients-lock)
      (broadcast (sprintf "~a hat den Server verlassen" name)))))

(define (handle-client in out)
  (let ((lock (make-mutex)))
    (handle-exceptions e #f
      (let* ((pk (read-packet in)) (p (cdr pk)))
        (read-varint-port p)             ; Protokollversion
        (read-mc-string p)               ; Adresse
        (read-u16 p)                     ; Port
        (case (read-varint-port p)
          ((1) (handle-status in out lock))
          ((2) (handle-login in out lock)))))
    (close-input-port in)
    (close-output-port out)))

(define (main)
  (let ((listener (tcp-listen port)))
    (printf "CHICKEN-Minecraft-Server laeuft auf Port ~a (Minecraft 1.8.x)~%" port)
    (let loop ()
      (receive (in out) (tcp-accept listener)
        (thread-start! (make-thread (lambda () (handle-client in out)))))
      (loop))))

(tcp-read-timeout #f)
(main)
