(define-record-type udp* (fields fd h p))

;; any -> bool
(define udp:socket?
  udp*?)

;; string -> int -> socket
(define udp:open
  (lambda (h p)
    (let ((fd (ikarus:udp-open)))
      (ikarus:udp-connect fd h p)
      (make-udp* fd h p))))

;; socket -> bytevector -> ()
(define udp:send
  (lambda (u b)
    (ikarus:udp-send (udp*-fd u) b)))

;; socket -> maybe bytevector
(define udp:recv
  (lambda (u)
    (let* ((b (make-bytevector 8388608))
           (n (ikarus:udp-recv (udp*-fd u) b))
           (r (make-bytevector n)))
      (bytevector-copy! b 0 r 0 n)
      r)))

;; socket -> ()
(define udp:close
  (lambda (u)
    (ikarus:udp-close (udp*-fd u))))

(define-record-type tcp* (fields fd h p))

;; any -> bool
(define tcp:socket?
  tcp*?)

;; string -> int -> socket
(define tcp:open
  (lambda (h p)
    (let ((fd (ikarus:tcp-open)))
      (ikarus:tcp-connect fd h p)
      (make-tcp* fd h p))))

;; socket -> bytevector -> ()
(define tcp:send
  (lambda (u b)
    (ikarus:tcp-send (tcp*-fd u) b)))

;; socket -> int -> maybe bytevector
(define tcp:read
  (lambda (u n)
    (let* ((b (make-bytevector n))
           (n (ikarus:tcp-recv (tcp*-fd u) b))
           (r (make-bytevector n)))
      (bytevector-copy! b 0 r 0 n)
      r)))

;; socket -> ()
(define tcp:close
  (lambda (u)
    (ikarus:tcp-close (tcp*-fd u))))
