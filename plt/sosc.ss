(module sosc scheme/base

(require mzlib/include)
(require scheme/udp)
(require rnrs/bytevectors-6)
(require (only-in rnrs/io/ports-6 
		  get-bytevector-n 
		  get-u8 
		  lookahead-u8 
		  open-bytevector-input-port))
(require rhs/plt/rhs)

(define-struct udp* (s h p))

;; string -> int -> socket
(define (udp:open h p)
  (make-udp* (udp-open-socket) h p))

;; socket -> bytevector -> ()
(define (udp:send u b)
  (let ((s (udp*-s u))
	(h (udp*-h u))
	(p (udp*-p u)))
    (udp-send-to* s h p b)))

;; socket -> maybe bytevector
(define (udp:recv u)
  (let* ((s (udp*-s u))
	 (h (udp*-h u))
	 (p (udp*-p u))
	 (b (make-bytes 8192))
	 (r (sync/timeout 1.0 (udp-receive!-evt s b))))
    (if r
	(subbytes b 0 (car r))
	#f)))

;; socket -> ()
(define (udp:close u)
  (udp-close (udp*-s u)))

(define bytevector-ieee-single-ref 
  floating-point-bytes->real)

(define bytevector-ieee-double-ref 
  floating-point-bytes->real)

(define bytevector-ieee-single-set! 
  (lambda (v k x e) 
    (real->floating-point-bytes x 4 e v)))

(define bytevector-ieee-double-set! 
  (lambda (v k x e)
    (real->floating-point-bytes x 8 e v)))

(define-syntax define-record-type
  (syntax-rules (fields)
    ((_ name (fields f0 ...))
     (define-struct name (f0 ...) #:inspector (make-inspector)))))

(provide (all-defined-out))
(include "../src/sosc.scm")
)
