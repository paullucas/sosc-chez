;; bytevector -> (() -> any) -> any
(define with-input-from-bytevector 
  (lambda (b f)
    (parameterize
     ((current-input-port (open-bytevector-input-port b)))
     (f))))

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

;; float -> int
(define floor-exact
  (compose inexact->exact floor))

;; float -> int
(define round-exact
  (compose inexact->exact round))

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
  (lambda ()
    (let* ((p (current-input-port))
	   (n (lookahead-u8 p))
	   (v (read-bstr (+ n 1))))
      (decode-pstr v))))

(define read-cstr
  (lambda ()
    (let loop ((l nil)
	       (b (get-u8 (current-input-port))))
      (if (= b 0)
	  (list->string (map integer->char (reverse l)))
	  (loop (cons b l) (get-u8 (current-input-port)))))))

;; int -> bytevector
(define read-bstr
  (lambda (n)
    (get-bytevector-n (current-input-port) n)))

(define read-i16
  (lambda () 
    (decode-i16 (read-bstr 2))))

(define read-u16
  (lambda () 
    (decode-u16 (read-bstr 2))))

(define read-i32 
  (lambda () 
    (decode-i32 (read-bstr 4))))

(define read-u32 
  (lambda () 
    (decode-u32 (read-bstr 4))))

(define read-i64 
  (lambda () 
    (decode-i64 (read-bstr 8))))

(define read-u64 
  (lambda () 
    (decode-u64 (read-bstr 8))))

(define read-f32 
  (lambda () 
    (decode-f32 (read-bstr 4))))

(define read-f64 
  (lambda () 
    (decode-f64 (read-bstr 8))))


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

(define 2^32
  (expt 2 32))

(define 2^32.
  (exact->inexact 2^32))

(define seconds-from-1900-to-1970
  (+ (* 70 365 24 60 60) (* 17 24 60 60)))

(define ntpr->ntp
  (lambda (i)
    (round-exact (* i 2^32))))

(define ntp-to-seconds
  (lambda (i)
    (/ i 2^32)))

(define ntp-to-seconds. 
  (lambda (i)
    (/ i 2^32.)))

(define nanoseconds-to-ntp
  (lambda (i)
    (round-exact (* i (/ 2^32 (expt 10 9))))))

(define ntp-to-nanoseconds
  (lambda (i)
    (* i (/ (expt 10 9) 2^32))))

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
  (lambda ()
    (let* ((s (read-cstr))
	   (n (modulo (cstring-length s) 4))
	   (p (- 4 (modulo n 4))))
      (if (not (= n 0))
	  (read-bstr p)
	  #f)
      s)))

;; OSC byte strings are length prefixed.

(define read-obyt
  (lambda ()
    (let* ((n (read-i32))
	   (b (read-bstr n))
	   (p (- 4 (modulo n 4))))
      (if (not (= n 0))
	  (read-bstr p)
	  #f)
      b)))

;; Evaluates to the object, described by the OSC type character
;; `type', encoded at the OSC byte stream `p'.

(define read-value
  (lambda (t)
    (cond
     ((eq? t oI32) (read-i32))
     ((eq? t oI64) (read-i64))
     ((eq? t oU64) (read-u64))
     ((eq? t oF32) (read-f32))
     ((eq? t oF64) (read-f64))
     ((eq? t oStr) (read-ostr))
     ((eq? t oByt) (read-obyt))
     (else (error 'read-value "bad type" t)))))

;; Evaluate to the list of objects encoded at the OSC byte stream
;; `p', conforming to the types given in the OSC character type
;; list `types'.

(define read-arguments
  (lambda (types)
    (if (null? types)
	'()
	(cons (read-value (car types))
	      (read-arguments (cdr types))))))

;; Evaluate to the scheme representation of the OSC message at the OSC
;; byte stream `p'. The first object is the 'address' of the
;; message, any subsequent objects are arguments for that address.

(define read-message
  (lambda ()
    (let* ((address (read-ostr))
	   (types (read-ostr)))
      (cons address
	    (read-arguments (cdr (string->list types)))))))

;; Evaluate to a scheme representation of the OSC bundle encoded at
;; the OSC stream `p'. The bundle ends at the end of the byte
;; stream. The first object is the <real> UTC 'timetag' of the
;; bundle, any subsequent objects are either OSC messages or embedded
;; OSC bundles.

(define read-bundle
  (lambda ()
    (let ((bundletag (read-ostr))
	  (timetag (ntp->utc. (read-u64)))
	  (parts (list)))
      (if (not (equal? bundletag "#bundle"))
	  (error 'read-bundle "illegal bundle tag" bundletag)
	  (cons timetag
		(let loop ((parts (list)))
		  (if (eof-object? (lookahead-u8 (current-input-port)))
		      (reverse parts)
		      (begin
			;; We have no use for the message size...
			(read-i32)
			(loop (cons (read-packet) parts))))))))))

;; byte
(define hash-u8
  (char->integer #\#))

;; () -> osc
(define read-packet
  (lambda () 
    (if (eq? (lookahead-u8 (current-input-port)) hash-u8)
	(read-bundle)
	(read-message))))

;; bytevector -> osc
(define decode-osc
  (lambda (b)
    (with-input-from-bytevector b read-packet)))

;; [byte] -> ()
(define osc-display
  (lambda (l)
    (for-each
     (lambda (b n)
       (display (format "~a (~a)" (number->string b 16) (integer->char b)))
       (if (= 3 (modulo n 4))
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
    (let ((n (modulo (cstring-length s) 4)))
      (list (encode-cstr s)
	    (if (= n 0)
		(list)
		(padding-of (- 4 n)))))))

;; bytevector -> [bytevector]
(define encode-bytes
  (lambda (b)
    (let* ((n (bytevector-length b))
	   (n* (modulo n 4)))
      (list (encode-i32 n)
	    b
	    (if (= n* 0)
		(list)
		(padding-of (- 4 n*)))))))

;; any -> bytevector|[bytevector]
(define encode-value
  (lambda (e)
    (cond ((exact-integer? e) (encode-i32 e))
	  ((real? e) (encode-f32 e))
	  ((string? e) (encode-string e))
	  ((bytevector? e) (encode-bytes e))
	  (else (error 'encode-value "illegal value" e)))))

;; [any] -> [bytevector]
(define encode-types
  (lambda (l)
    (encode-string
     (list->string
      (cons #\,
	    (map (lambda (e)
		   (cond ((exact-integer? e) #\i)
			 ((real? e) #\f)
			 ((string? e) #\s)
			 ((bytevector? e) #\b)
			 (else (error 'encode-types "type?" e))))
		 l))))))

;; osc -> [bytevector]
(define encode-message
  (lambda (m)
    (list (encode-string (car m))
	  (encode-types (cdr m))
	  (map encode-value (cdr m)))))

;; osc -> [bytevector]
(define encode-bundle-ntp
  (lambda (b)
    (list (encode-string "#bundle")
	  (encode-u64 (ntpr->ntp (car b)))
	  (map (lambda (e)
		 (if (message? e)
		     (encode-bytes (encode-osc e))
		     (error 'encode-bundle "illegal value" e)))
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
    (cond ((or (number? e) (string? e) (bytevector? e)) e)
	  ((list? e) (map purify e))
	  ((symbol? e) (symbol->string e))
	  ((boolean? e) (if e 1 0))
	  (else (error 'purify "illegal input" e)))))

;; socket -> osc -> ()
(define send
  (lambda (u m)
    (udp:send u (encode-osc m))))

;; port -> maybe osc
(define recv
  (lambda (u)
    (let ((b (udp:recv u)))
      (if b (decode-osc b) #f))))

;; port -> string -> osc -> float -> mabye osc
(define osc-request
  (lambda (fd r m t)
    (send fd m)
    (let ((p (recv fd t)))
      (if (and p (string=? (head p) r)) 
	  p 
	  #f))))

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
	(error "message: illegal address"))))

;; float -> [any] -> osc
(define bundle
  (lambda (t l)
    (if (number? t)
	(cons t l)
	(error "bundle: illegal timestamp" t))))

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
    (and (string? (car m))
	 (all (lambda (e) (or (integer? e)
			      (real? e)
			      (string? e)))
	      (cdr m)))))

;; osc -> bool
(define verify-bundle
  (lambda (b)
    (and (integer? (car b))
	 (all (lambda (e) (or (verify-message e)
			      (and (verify-bundle e)
				   (>= (car e) (car b)))))
	      (cdr b)))))

;; osc -> bool
(define verify-packet
  (lambda (p)
    (or (verify-message p)
	(verify-bundle p))))
