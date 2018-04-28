#!r6rs
(library (sosc)
  
  (export udp* make-udp* udp*? udp*-s udp*-h udp*-p udp:socket? udp:open udp:send udp:recv udp:close
          tcp* make-tcp* tcp*? tcp*-s tcp*-h tcp*-p tcp:socket? tcp:open tcp:send tcp:read tcp:close
          with-input-from-bytevector bytevector-section bytevector-find-index flatten-bytevectors
          bytevector-make-and-set1 bytevector-make-and-set decode-u8 decode-u16 decode-u32 decode-u64
          decode-i8 decode-i16 decode-i32 decode-i64 decode-f32 decode-f64 decode-str decode-pstr
          decode-cstr encode-u8 encode-u16 encode-u32 encode-u64 encode-i8 encode-i16 encode-i32
          encode-i64 encode-f32 encode-f64 encode-str encode-pstr encode-cstr read-bstr read-pstr
          read-cstr read-i8 read-u8 read-i16 read-u16 read-i32 read-u32 read-i64 read-u64 read-f32
          read-f64 seconds-from-1900-to-1970 ntpr->ntp utc->ntpr ntp->utc read-ostr read-obyt read-value
          read-arguments read-message read-bundle hash-u8 read-packet decode-osc osc-display cstring-length
          osc-align padding-of encode-string encode-bytes encode-value encode-types encode-message
          encode-bundle-ntp encode-bundle encode-osc purify oI32 oI64 oU64 oF32 oF64 oSTR oBYT oMID message
          bundle message? bundle? verify-message verify-bundle verify-packet send recv wait
          ;; (send fd m) (recv fd) (wait fd s)
          )
  
  (import (rnrs)
          (rhs)
          (socket ffi)
          (socket socket)
          (libc libc))
  
  (define-record-type udp*
    (fields s h p))

  ;; any -> bool
  (define udp:socket? udp*?)

  ;; string -> int -> socket
  (define (udp:open h p)
    (let ((s (socket AF_INET SOCK_DGRAM 0)))
      (socket:connect s AF_INET h p)
      (make-udp* s h p)))

  ;; socket -> bytevector -> ()
  (define (udp:send t b)
    (let ((s (udp*-s t)))
      (socket:write s b)))

  ;; socket -> int -> maybe bytevector
  (define (udp:recv u)
    (let* ((l (* 8192 4))
           (b (make-bytevector l)))
      (socket:read (udp*-s u) l b)
      b))

  ;; socket -> ()
  (define (udp:close t)
    (socket:close (udp*-s t)))
  
  (define-record-type tcp* (fields s h p))

  ;; any -> bool
  (define tcp:socket? tcp*?)

  ;; string -> int -> socket
  (define (tcp:open h p)
    (let ((s (socket AF_INET SOCK_STREAM 0)))
      (socket:connect s AF_INET h p)
      (make-tcp* s h p)))

  ;; socket -> bytevector -> ()
  (define (tcp:send t b)
    (let ((s (tcp*-s t)))
      (socket:write s b)))

  ;; socket -> int -> maybe bytevector
  (define (tcp:read u n)
    (let ((b (make-bytevector n)))
      (socket:read (tcp*-s u) n b)
      b))

  ;; socket -> ()
  (define (tcp:close t)
    (socket:close (tcp*-s t)))

  ;; bytevector -> (port -> any) -> any
  (define (with-input-from-bytevector b f)
    (let* ((p (open-bytevector-input-port b))
           (r (f p)))
      (close-port p)
      r))

  ;; bytevector -> int -> int -> bytevector
  (define (bytevector-section v l r)
    (let* ((n (- r l))
           (w (make-bytevector n 0)))
      (bytevector-copy! v l w 0 n)
      w))

  ;; bytevector -> byte -> int
  (define (bytevector-find-index v x)
    (letrec ((f (lambda (i)
                  (if (= (bytevector-u8-ref v i) x)
                      i
                      (f (+ i 1))))))
      (f 0)))

  ;; Tree bytevector -> bytevector
  (define (flatten-bytevectors t)
    (let* ((l (flatten t))
           (n (map bytevector-length l))
           (m (sum n))
           (v (make-bytevector m)))
      (let loop ((i 0) (l l) (n n))
        (if (null? l)
            v
            (let ((l0 (car l))
                  (n0 (car n)))
              (bytevector-copy! l0 0 v i n0)
              (loop (+ i n0) (cdr l) (cdr n)))))))

  ;; number a => (bytevector -> int -> a -> ()) -> int -> a
  (define (bytevector-make-and-set1 f k n)
    (let ((v (make-bytevector k 0)))
      (f v 0 n)
      v))

  ;; number a => (bytevector -> int -> a -> ()) -> int -> a
  (define (bytevector-make-and-set f k n)
    (let ((v (make-bytevector k 0)))
      (f v 0 n (endianness big))
      v))

  ;; bytevector -> int
  (define (decode-u8 v)
    (bytevector-u8-ref v 0))

  ;; bytevector -> int
  (define (decode-u16 v)
    (bytevector-u16-ref v 0 (endianness big)))

  ;; bytevector -> int
  (define (decode-u32 v)
    (bytevector-u32-ref v 0 (endianness big)))

  ;; bytevector -> int
  (define (decode-u64 v)
    (bytevector-u64-ref v 0 (endianness big)))

  ;; bytevector -> int
  (define (decode-i8 v)
    (bytevector-s8-ref v 0))

  ;; bytevector -> int
  (define (decode-i16 v)
    (bytevector-s16-ref v 0 (endianness big)))

  ;; bytevector -> int
  (define (decode-i32 v)
    (bytevector-s32-ref v 0 (endianness big)))

  ;; bytevector -> int
  (define (decode-i64 v)
    (bytevector-s64-ref v 0 (endianness big)))

  ;; bytevector -> double
  (define (decode-f32 v)
    (bytevector-ieee-single-ref v 0 (endianness big)))

  ;; bytevector -> double
  (define (decode-f64 v)
    (bytevector-ieee-double-ref v 0 (endianness big)))

  ;; bytevector -> string
  (define (decode-str b)
    (utf8->string b))

  ;; bytevector -> string
  ;;
  ;; (decode-pstr (flatten-bytevectors (encode-pstr "string")))
  (define (decode-pstr v)
    (let* ((n (decode-u8 v))
           (w (bytevector-section v 1 (+ n 1))))
      (decode-str w)))

  ;; bytevector -> string
  (define (decode-cstr v)
    (let* ((n (bytevector-find-index v 0))
           (w (bytevector-section v 0 n)))
      (decode-str w)))

  ;; int -> bytevector
  (define (encode-u8 n)
    (bytevector-make-and-set1 bytevector-u8-set! 1 (exact n)))

  ;; int -> bytevector
  (define (encode-u16 n)
    (bytevector-make-and-set bytevector-u16-set! 2 (exact n)))

  ;; int -> bytevector
  (define (encode-u32 n)
    (bytevector-make-and-set bytevector-u32-set! 4 (exact n)))

  ;; int -> bytevector
  (define (encode-u64 n)
    (bytevector-make-and-set bytevector-u64-set! 8 (exact n)))

  ;; int -> bytevector
  (define (encode-i8 n)
    (bytevector-make-and-set1 bytevector-s8-set! 1 (exact n)))

  ;; int -> bytevector
  (define (encode-i16 n)
    (bytevector-make-and-set bytevector-s16-set! 2 (exact n)))

  ;; int -> bytevector
  (define (encode-i32 n)
    (bytevector-make-and-set bytevector-s32-set! 4 (exact n)))

  ;; int -> bytevector
  (define (encode-i64 n)
    (bytevector-make-and-set bytevector-s64-set! 8 (exact n)))

  ;; double -> bytevector
  (define (encode-f32 n)
    (bytevector-make-and-set bytevector-ieee-single-set! 4 (inexact n)))

  ;; double -> bytevector
  (define (encode-f64 n)
    (bytevector-make-and-set bytevector-ieee-double-set! 8 (inexact n)))

  ;; string -> bytevector
  (define (encode-str s)
    (string->utf8 s))

  ;; string -> bytevector
  (define (encode-pstr s)
    (let* ((b (encode-str s))
           (n (encode-u8 (bytevector-length b))))
      (list n b)))

  ;; string -> [bytevector]
  (define (encode-cstr s)
    (let* ((b (encode-str s))
           (z (encode-u8 0)))
      (list b z)))

  ;; port -> int -> bytevector
  (define (read-bstr p n)
    (get-bytevector-n p n))

  ;; port -> string
  (define (read-pstr p)
    (let* ((n (lookahead-u8 p))
           (v (read-bstr p (+ n 1))))
      (decode-pstr v)))

  ;; port -> string
  (define (read-cstr p)
    (let loop ((l nil)
               (b (get-u8 p)))
      (if (= b 0)
          (list->string (map integer->char (reverse l)))
          (loop (cons b l) (get-u8 p)))))

  ;; port -> int
  (define (read-i8 p)
    (decode-i8 (read-bstr p 1)))

  ;; port -> int
  (define (read-u8 p)
    (decode-u8 (read-bstr p 1)))

  ;; port -> int
  (define (read-i16 p)
    (decode-i16 (read-bstr p 2)))

  ;; port -> int
  (define (read-u16 p)
    (decode-u16 (read-bstr p 2)))

  ;; port -> int
  (define (read-i32 p)
    (decode-i32 (read-bstr p 4)))

  ;; port -> int
  (define (read-u32 p)
    (decode-u32 (read-bstr p 4)))

  ;; port -> int
  (define (read-i64 p)
    (decode-i64 (read-bstr p 8)))

  ;; port -> int
  (define (read-u64 p)
    (decode-u64 (read-bstr p 8)))

  ;; port -> double
  (define (read-f32 p)
    (decode-f32 (read-bstr p 4)))

  ;; port -> double
  (define (read-f64 p)
    (decode-f64 (read-bstr p 8)))

  ;; int
  (define seconds-from-1900-to-1970
    (+ (* 70 365 24 60 60)
       (* 17 24 60 60)))

  ;; double -> int
  (define (ntpr->ntp n)
    (exact (round (* n (expt 2 32)))))

  ;; double -> double
  (define (utc->ntpr n)
    (+ n seconds-from-1900-to-1970))

  ;; int -> double
  (define (ntp->utc n)
    (- (/ n (expt 2 32))
       seconds-from-1900-to-1970))

  ;; port -> string
  (define (read-ostr p)
    (let* ((s (read-cstr p))
           (n (mod (cstring-length s) 4))
           (i (- 4 (mod n 4))))
      (if (not (= n 0))
          (read-bstr p i)
          #f)
      s))

  ;; port -> bytevector
  (define (read-obyt p)
    (let* ((n (read-i32 p))
           (b (read-bstr p n))
           (i (- 4 (mod n 4))))
      (if (not (= n 0))
          (read-bstr p i)
          #f)
      b))

  ;; datum = int | double | string | bytevector

  ;; port -> char -> datum
  (define (read-value p t)
    (cond ((equal? t oI32) (read-i32 p))
          ((equal? t oI64) (read-i64 p))
          ((equal? t oU64) (read-u64 p))
          ((equal? t oF32) (read-f32 p))
          ((equal? t oF64) (read-f64 p))
          ((equal? t oSTR) (read-ostr p))
          ((equal? t oBYT) (read-obyt p))
          ((equal? t oMID) (read-u32 p))
          (else (error "read-value" "bad type" t))))

  ;; port -> [char] -> [datum]
  (define (read-arguments p types)
    (if (null? types)
        (quote ())
        (cons (read-value p (car types))
              (read-arguments p (cdr types)))))

  ;; port -> (string:[datum])
  (define (read-message p)
    (let* ((address (read-ostr p)) (types (read-ostr p)))
      (cons address (read-arguments p (cdr (string->list types))))))

  ;; port -> (utc:[message])
  (define (read-bundle p)
    (let ((bundletag (read-ostr p))
          (timetag (ntp->utc (read-u64 p)))
          (parts (list)))
      (if (not (equal? bundletag "#bundle"))
          (error "read-bundle" "illegal bundle tag" bundletag)
          (cons timetag
                (let loop
                    ((parts (list)))
                  (if (eof-object? (lookahead-u8 p))
                      (reverse parts)
                      (begin (read-i32 p)
                             (loop (cons (read-packet p) parts)))))))))

  ;; byte
  (define hash-u8 (char->integer #\#))

  ;; port -> osc
  (define (read-packet p)
    (if (equal? (lookahead-u8 p) hash-u8)
        (read-bundle p)
        (read-message p)))

  ;; bytevector -> osc
  (define (decode-osc b)
    (with-input-from-bytevector b read-packet))

  ;; [byte] -> ()
  (define (osc-display l)
    (zip-with (lambda (b n)
                (display (list (number->string b 16) (integer->char b)))
                (if (= 3 (mod n 4))
                    (newline)
                    (display #\space)))
              l
              (enum-from-to 0 (- (length l) 1))))

  ;; string -> int
  (define (cstring-length s)
    (+ 1 (string-length s)))

  ;; int -> int
  ;; (equal? (map osc-align (enum-from-to 0 7)) (list 0 3 2 1 0 3 2 1))
  (define (osc-align n)
    (- (fxand (+ n 3)
              (fxnot 3))
       n))

  ;; int -> [bytevector]
  (define (padding-of n)
    (replicate (osc-align n)
               (encode-u8 0)))

  ;; string -> [bytevector]
  (define (encode-string s)
    (list (encode-cstr s)
          (padding-of (cstring-length s))))

  ;; bytevector -> [bytevector]
  (define (encode-bytes b)
    (let ((n (bytevector-length b)))
      (list (encode-i32 n) b (padding-of n))))

  ;; datum -> bytevector
  (define (encode-value e)
    (cond ((number? e) (if (integer? e) (encode-i32 e) (encode-f32 e)))
          ((string? e) (encode-string e))
          ((bytevector? e) (encode-bytes e))
          (else (error "encode-value" "illegal value" e))))

  ;; [datum] -> bytevector
  (define (encode-types l)
    (encode-string
     (list->string
      (cons #\,
            (map (lambda (e)
                   (cond ((number? e) (if (integer? e) oI32 oF32))
                         ((string? e) oSTR)
                         ((bytevector? e) oBYT)
                         (else (error "encode-types" "type?" e))))
                 l)))))

  ;; osc -> [bytevector]
  (define (encode-message m)
    (list (encode-string (car m))
          (encode-types (cdr m))
          (map encode-value (cdr m))))

  ;; osc -> [bytevector]
  (define (encode-bundle-ntp b)
    (list (encode-string "#bundle")
          (encode-u64 (ntpr->ntp (car b)))
          (map (lambda (e)
                 (if (message? e)
                     (encode-bytes (encode-osc e))
                     (error "encode-bundle" "illegal value" e)))
               (cdr b))))

  ;; osc -> [bytevector]
  (define (encode-bundle b)
    (encode-bundle-ntp
     (cons (utc->ntpr (car b))
           (cdr b))))
  
  ;; osc -> bytevector
  (define (encode-osc p)
    (flatten-bytevectors
     (if (bundle? p)
         (encode-bundle p)
         (encode-message p))))

  ;; any | [any] -> datum | [datum]
  (define (purify e)
    (cond ((or3 (number? e)
                (string? e)
                (bytevector? e))
           e)
          ((list? e)
           (map purify e))
          ((symbol? e)
           (symbol->string e))
          ((boolean? e)
           (if e 1 0))
          (else (error "purify" "illegal input" e))))

  ;; char
  (define oI32 #\i)
  (define oI64 #\h)
  (define oU64 #\t)
  (define oF32 #\f)
  (define oF64 #\d)
  (define oSTR #\s)
  (define oBYT #\b)
  (define oMID #\m)

  ;; string -> [any] -> osc
  (define (message c l)
    (if (string? c)
        (cons c l)
        (error "message" "illegal address")))

  ;; float -> [any] -> osc
  (define (bundle t l)
    (if (number? t)
        (cons t l)
        (error "bundle" "illegal timestamp" t)))

  ;; osc -> bool
  (define (message? p)
    (string? (car p)))

  ;; osc -> bool
  (define (bundle? p)
    (number? (car p)))

  ;; osc -> bool
  (define (verify-message m)
    (and2 (string? (car m))
          (all (cdr m) (lambda (e)
                         (or (number? e) (string? e))))))

  ;; osc -> bool
  (define (verify-bundle b)
    (and2 (integer? (car b))
          (all (lambda (e)
                 (or2 (verify-message e)
                      (and2 (verify-bundle e)
                            (>= (car e) (car b)))))
               (cdr b))))

  ;; osc -> bool
  (define (verify-packet p)
    (or2 (verify-message p)
         (verify-bundle p)))

  ;; socket -> osc -> ()
  (define (send fd m)
    (let ((b (encode-osc m)))
      (cond ((udp:socket? fd)
             (udp:send fd b))
            ((tcp:socket? fd)
             (tcp:send fd (encode-u32 (bytevector-length b)))
             (tcp:send fd b)))))

  ;; port -> maybe osc
  (define (recv fd)
    (cond ((udp:socket? fd)
           (let ((b (udp:recv fd)))
             (and2 b (decode-osc b))))
          ((tcp:socket? fd)
           (let* ((b (tcp:read fd 4))
                  (n (decode-u32 b)))
             (decode-osc (tcp:read fd n))))))

  ;; port -> string -> osc
  (define (wait fd s)
    (let ((p (recv fd)))
      (cond ((not p)
             (error "wait" "timed out"))
            ((not (string=? (head p) s))
             (error "wait" "bad return packet" p s))
            (else p)))))
