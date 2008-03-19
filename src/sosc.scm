;; bytevector -> (port -> any) -> any
(define with-input-from-bytevector 
  (lambda (b f)
    (let* ((p (open-bytevector-input-port b))
	   (r (f p)))
      (close-port p)
      r)))

;; bytevector -> int -> int -> bytevector
(define bytevector-section
  (lambda (v l r)
    (let* ((n (- r l))
	   (w (make-bytevector n 0)))
      (bytevector-copy! v l w 0 n)
      w)))

;; bytevector -> byte -> int
(define bytevector-find-index
  (lambda (v x)
    (letrec ((f (lambda (i)
		  (if (= (bytevector-u8-ref v i) x)
		      i
		      (f (+ i 1))))))
      (f 0))))

;; Tree bytevector -> bytevector
(define flatten-bytevectors
  (lambda (t)
    (let* ((l (flatten t))
	   (n (map1 bytevector-length l))
	   (m (sum n))
	   (v (make-bytevector m)))
      (let loop ((i 0)
		 (l l)
		 (n n))
	(if (null? l)
	    v
	    (let ((l0 (car l))
		  (n0 (car n)))
	      (bytevector-copy! l0 0 v i n0)
	      (loop (+ i n0) (cdr l) (cdr n))))))))

;; (bytevector -> int -> x) -> int -> x
(define bytevector-make-and-set1
  (lambda (f k n)
    (let ((v (make-bytevector k 0)))
      (f v 0 n)
      v)))

;; (bytevector -> int -> x) -> int -> x
(define bytevector-make-and-set
  (lambda (f k n)
    (let ((v (make-bytevector k 0)))
      (f v 0 n be)
      v)))

(define decode-u8
  (lambda (v) 
    (bytevector-u8-ref v 0)))

(define decode-u16
  (lambda (v) 
    (bytevector-u16-ref v 0 be)))

(define decode-u32
  (lambda (v) 
    (bytevector-u32-ref v 0 be)))

(define decode-u64
  (lambda (v) 
    (bytevector-u64-ref v 0 be)))

(define decode-i8
  (lambda (v) 
    (bytevector-s8-ref v 0)))

(define decode-i16
  (lambda (v) 
    (bytevector-s16-ref v 0 be)))

(define decode-i32
  (lambda (v)
    (bytevector-s32-ref v 0 be)))

(define decode-i64
  (lambda (v)
    (bytevector-s64-ref v 0 be)))

(define decode-f32
  (lambda (v) 
    (bytevector-ieee-single-ref v 0)))

(define decode-f64
  (lambda (v) 
    (bytevector-ieee-double-ref v 0)))

(define decode-str
  (lambda (b)
    (utf8->string b)))

(define decode-pstr
  (lambda (v)
    (let* ((n (decode-u8 v))
	   (w (bytevector-section v 1 (+ n 1))))
      (decode-str w))))

(define decode-cstr
  (lambda (v)
    (let* ((n (bytevector-find-index v 0))
	   (w (bytevector-section v 0 n)))
      (decode-str w))))

(define be (endianness big))

(define encode-u8
  (lambda (n) 
    (bytevector-make-and-set1 bytevector-u8-set! 1 n)))

(define encode-u16
  (lambda (n)
    (bytevector-make-and-set bytevector-u16-set! 2 n)))

(define encode-u32
  (lambda (n)
    (bytevector-make-and-set bytevector-u32-set! 4 n)))

(define encode-u64
  (lambda (n)
    (bytevector-make-and-set bytevector-u64-set! 8 n)))

(define encode-i8
  (lambda (n) 
    (bytevector-make-and-set1 bytevector-s8-set! 1 n)))

(define encode-i16
  (lambda (n)
    (bytevector-make-and-set bytevector-s16-set! 2 n)))

(define encode-i32
  (lambda (n) 
    (bytevector-make-and-set bytevector-s32-set! 4 n)))

(define encode-i64
  (lambda (n)
    (bytevector-make-and-set bytevector-s64-set! 8 n)))

(define encode-f32
  (lambda (n)
    (bytevector-make-and-set bytevector-ieee-single-set! 4 n)))

(define encode-f64
  (lambda (n)
    (bytevector-make-and-set bytevector-ieee-double-set! 8 n)))

(define encode-str
  (lambda (s)
    (string->utf8 s)))

(define encode-pstr
  (lambda (s)
    (let* ((b (encode-str s))
	   (n (encode-u8 (bytevector-length b))))
      (list n b))))

(define encode-cstr
  (lambda (s)
    (let* ((b (encode-str s))
	   (z (encode-u8 0)))
      (list b z))))

(define read-pstr
  (lambda (p)
    (let* ((n (lookahead-u8 p))
	   (v (read-bstr p (+ n 1))))
      (decode-pstr v))))

(define read-cstr
  (lambda (p)
    (let loop ((l nil)
	       (b (get-u8 p)))
      (if (= b 0)
	  (list->string (map1 integer->char (reverse l)))
	  (loop (cons b l) (get-u8 p))))))

;; int -> bytevector
(define read-bstr
  (lambda (p n)
    (get-bytevector-n p n)))

(define read-i16
  (lambda (p) 
    (decode-i16 (read-bstr p 2))))

(define read-u16
  (lambda (p) 
    (decode-u16 (read-bstr p 2))))

(define read-i32 
  (lambda (p) 
    (decode-i32 (read-bstr p 4))))

(define read-u32 
  (lambda (p) 
    (decode-u32 (read-bstr p 4))))

(define read-i64 
  (lambda (p) 
    (decode-i64 (read-bstr p 8))))

(define read-u64 
  (lambda (p) 
    (decode-u64 (read-bstr p 8))))

(define read-f32 
  (lambda (p) 
    (decode-f32 (read-bstr p 4))))

(define read-f64 
  (lambda (p) 
    (decode-f64 (read-bstr p 8))))


;; ntp

;; NTP is the Network Time Protocol. NTP time is represented by a 64
;; bit fixed point number. The first 32 bits specify the number of
;; seconds since midnight on January 1, 1900, and the last 32 bits
;; specify fractional parts of a second to a precision of about 200
;; picoseconds. This is the representation used by Internet NTP
;; timestamps.

;; The number of seconds from the start of 1900 to the start of 1970.
;; NTP is measured from the former, UTC from the latter. There are 17
;; leap years in this period.

(define seconds-from-1900-to-1970
  (+ (* 70 365 24 60 60) (* 17 24 60 60)))

(define ntpr->ntp
  (lambda (i)
    (round (* i (expt 2 32)))))

(define ntp-to-seconds
  (lambda (i)
    (/ i (expt 2 32))))

(define ntp-to-seconds. 
  (lambda (i)
    (/ i (expt 2 32))))

(define nanoseconds-to-ntp
  (lambda (i)
    (round (* i (/ (expt 2 32) (expt 10 9))))))

(define ntp-to-nanoseconds
  (lambda (i)
    (* i (/ (expt 10 9) (expt 2 32)))))

;; Convert between time intervals in seconds and NTP intervals.

(define time-interval->ntp-interval
  (lambda (interval)
    (ntpr->ntp interval)))

(define ntp-interval->time-interval
  (lambda (ntp-interval)
    (ntp-to-seconds ntp-interval)))

(define ntp-interval->time-interval. 
  (lambda (ntp-interval)
    (ntp-to-seconds. ntp-interval)))

;; Evaluate to an <real> representing the NTP time of the UTC time of
;; the <real> number `n'.

(define utc->ntpr
  (lambda (n)
    (+ n seconds-from-1900-to-1970)))

;; Evaluate to an <integer> representing the NTP time of the UTC time
;; of the <real> number `n'.

(define utc->ntp
  (lambda (n)
    (ntpr->ntp (+ n seconds-from-1900-to-1970))))

;; Evaluate to a <real> number representing the UTC time of the
;; <integer> NTP time `ntp'.

(define ntp->utc
  (lambda (ntp)
    (- (ntp-to-seconds ntp) seconds-from-1900-to-1970)))

(define ntp->utc. 
  (lambda (ntp)
    (- (ntp-to-seconds. ntp) seconds-from-1900-to-1970)))

;; OSC strings are C strings padded to a four byte boundary.

(define read-ostr
  (lambda (p)
    (let* ((s (read-cstr p))
	   (n (mod (cstring-length s) 4))
	   (i (- 4 (mod n 4))))
      (if (not (= n 0))
	  (read-bstr p i)
	  #f)
      s)))

;; OSC byte strings are length prefixed.

(define read-obyt
  (lambda (p)
    (let* ((n (read-i32 p))
	   (b (read-bstr p n))
	   (i (- 4 (mod n 4))))
      (if (not (= n 0))
	  (read-bstr p i)
	  #f)
      b)))

;; Evaluates to the object, described by the OSC type character
;; `type', encoded at the OSC byte stream `p'.

(define read-value
  (lambda (p t)
    (cond
     ((equal? t oI32) (read-i32 p))
     ((equal? t oI64) (read-i64 p))
     ((equal? t oU64) (read-u64 p))
     ((equal? t oF32) (read-f32 p))
     ((equal? t oF64) (read-f64 p))
     ((equal? t oStr) (read-ostr p))
     ((equal? t oByt) (read-obyt p))
     (else (error "read-value" "bad type" t)))))

;; Evaluate to the list of objects encoded at the OSC byte stream
;; `p', conforming to the types given in the OSC character type
;; list `types'.

(define read-arguments
  (lambda (p types)
    (if (null? types)
	'()
	(cons (read-value p (car types))
	      (read-arguments p (cdr types))))))

;; Evaluate to the scheme representation of the OSC message at the OSC
;; byte stream `p'. The first object is the 'address' of the
;; message, any subsequent objects are arguments for that address.

(define read-message
  (lambda (p)
    (let* ((address (read-ostr p))
	   (types (read-ostr p)))
      (cons address
	    (read-arguments p (cdr (string->list types)))))))

;; Evaluate to a scheme representation of the OSC bundle encoded at
;; the OSC stream `p'. The bundle ends at the end of the byte
;; stream. The first object is the <real> UTC 'timetag' of the
;; bundle, any subsequent objects are either OSC messages or embedded
;; OSC bundles.

(define read-bundle
  (lambda (p)
    (let ((bundletag (read-ostr p))
	  (timetag (ntp->utc. (read-u64 p)))
	  (parts (list)))
      (if (not (equal? bundletag "#bundle"))
	  (error "read-bundle" "illegal bundle tag" bundletag)
	  (cons timetag
		(let loop ((parts (list)))
		  (if (eof-object? (lookahead-u8 p))
		      (reverse parts)
		      (begin
			;; We have no use for the message size...
			(read-i32 p)
			(loop (cons (read-packet p) parts))))))))))

;; byte
(define hash-u8
  (char->integer #\#))

;; () -> osc
(define read-packet
  (lambda (p) 
    (if (equal? (lookahead-u8 p) hash-u8)
	(read-bundle p)
	(read-message p))))

;; bytevector -> osc
(define decode-osc
  (lambda (b)
    (with-input-from-bytevector b read-packet)))

;; [byte] -> ()
(define osc-display
  (lambda (l)
    (zip-with
     (lambda (b n)
       (display (list (number->string b 16) (integer->char b)))
       (if (= 3 (mod n 4))
	   (newline)
	   (display #\space)))
     l
     (enum-from-to 0 (- (length l) 1)))))

;; int -> [bytevector]
(define padding-of
  (lambda (n) (replicate n (encode-u8 0))))

;; string -> int
(define cstring-length
  (lambda (s)
    (+ 1 (string-length s))))

;; string -> [bytevector]
(define encode-string
  (lambda (s)
    (let ((n (mod (cstring-length s) 4)))
      (list (encode-cstr s)
	    (if (= n 0)
		(list)
		(padding-of (- 4 n)))))))

;; bytevector -> [bytevector]
(define encode-bytes
  (lambda (b)
    (let* ((n (bytevector-length b))
	   (n* (mod n 4)))
      (list (encode-i32 n)
	    b
	    (if (= n* 0)
		(list)
		(padding-of (- 4 n*)))))))

;; any -> bytevector|[bytevector]
(define encode-value
  (lambda (e)
    (cond ((integer? e) (encode-i32 e))
	  ((real? e) (encode-f32 e))
	  ((string? e) (encode-string e))
	  ((bytevector? e) (encode-bytes e))
	  (else (error "encode-value" "illegal value" e)))))

;; [any] -> [bytevector]
(define encode-types
  (lambda (l)
    (encode-string
     (list->string
      (cons #\,
	    (map1 (lambda (e)
		    (cond ((integer? e) #\i)
			  ((real? e) #\f)
			  ((string? e) #\s)
			  ((bytevector? e) #\b)
			  (else (error "encode-types" "type?" e))))
		 l))))))

;; osc -> [bytevector]
(define encode-message
  (lambda (m)
    (list (encode-string (car m))
	  (encode-types (cdr m))
	  (map1 encode-value (cdr m)))))

;; osc -> [bytevector]
(define encode-bundle-ntp
  (lambda (b)
    (list (encode-string "#bundle")
	  (encode-u64 (ntpr->ntp (car b)))
	  (map1 (lambda (e)
		  (if (message? e)
		      (encode-bytes (encode-osc e))
		      (error "encode-bundle" "illegal value" e)))
		(cdr b)))))

;; osc -> [bytevector]
(define encode-bundle
  (lambda (b)
    (encode-bundle-ntp (cons (utc->ntpr (car b)) (cdr b)))))

;; osc -> bytevector
(define encode-osc
  (lambda (p)
    (flatten-bytevectors
     (if (bundle? p)
	 (encode-bundle p)
	 (encode-message p)))))

;; any|[any] -> number|string|bytevector|[number|string|bytevector]
(define purify
  (lambda (e)
    (cond ((or3 (number? e) (string? e) (bytevector? e)) e)
	  ((list? e) (map1 purify e))
	  ((symbol? e) (symbol->string e))
	  ((boolean? e) (if e 1 0))
	  (else (error "purify" "illegal input" e)))))

;; socket -> osc -> ()
(define send
  (lambda (fd m)
    (let ((b (encode-osc m)))
      (cond ((udp:socket? fd)
	     (udp:send fd b))
	    ((tcp:socket? fd)
	     (tcp:send fd (encode-u32 (bytevector-length b)))
	     (tcp:send fd b))))))

;; port -> maybe osc
(define recv
  (lambda (fd)
    (cond ((udp:socket? fd)
	   (let ((b (udp:recv fd)))
	     (and2 b (decode-osc b))))
	  ((tcp:socket? fd)
	   (let ((n (decode-u32 (tcp:read fd 4))))
	     (display n)
	     (newline)
	     (decode-osc (tcp:read fd n)))))))
	  
;; char
(define oI32 #\i)
(define oI64 #\h)
(define oU64 #\t)
(define oF32 #\f)
(define oF64 #\d)
(define oStr #\s)
(define oByt #\b)

;; string -> [any] -> osc
(define message
  (lambda (c l)
    (if (string? c)
	(cons c l)
	(error "message" "illegal address"))))

;; float -> [any] -> osc
(define bundle
  (lambda (t l)
    (if (number? t)
	(cons t l)
	(error "bundle" "illegal timestamp" t))))

;; osc -> bool
(define message?
  (lambda (p)
    (string? (car p))))

;; osc -> bool
(define bundle?
  (lambda (p)
    (number? (car p))))

;; osc -> bool
(define verify-message
  (lambda (m)
    (and2 (string? (car m))
	  (all (lambda (e) (or3 (integer? e)
				(real? e)
				(string? e)))
	       (cdr m)))))

;; osc -> bool
(define verify-bundle
  (lambda (b)
    (and2 (integer? (car b))
	  (all (lambda (e) (or2 (verify-message e)
				(and2 (verify-bundle e)
				      (>= (car e) (car b)))))
	       (cdr b)))))

;; osc -> bool
(define verify-packet
  (lambda (p)
    (or2 (verify-message p)
	 (verify-bundle p))))
