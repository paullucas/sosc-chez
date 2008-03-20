(define-record-type udp* (fields fd h p))

;; any -> bool
(define udp:socket?
  udp*?)

;; string -> int -> socket
(define udp:open 
  (lambda (h p)
    (let ((fd (udp-open)))
      (udp-connect fd h p)
      (make-udp* fd h p))))

;; socket -> bytevector -> ()
(define udp:send 
  (lambda (u b)
    (udp-send (udp*-fd u) b)))

;; socket -> maybe bytevector
(define udp:recv
  (lambda (u)
    (let* ((b (make-bytevector 8388608))
	   (n (udp-recv (udp*-fd u) b))
	   (r (make-bytevector n)))
      (bytevector-copy! b 0 r 0 n)
      r)))

;; socket -> ()
(define udp:close 
  (lambda (u)
    (udp-close (udp*-fd u))))

(define-record-type tcp* (fields i o h p))

;; any -> bool
(define tcp:socket?
  tcp*?)

;; string -> int -> socket
(define tcp:open 
  (lambda (h p)
    (let-values
     (((o i) (tcp-connect h (number->string p))))
     ;;(output-port-buffer-mode (buffer-mode none))
     (make-tcp* i o h p))))

;; socket -> bytevector -> ()
(define tcp:send
  (lambda (fd b)
    (let ((o (tcp*-o fd)))
      (put-bytevector o b)
      (flush-output-port o))))
  
;; socket -> int -> maybe bytevector
(define tcp:read
  (lambda (fd n)
    (get-bytevector-n (tcp*-i fd) n)))

;; socket -> ()
(define tcp:close
  (lambda (fd)
    (close-port (tcp*-i fd))
    (close-port (tcp*-o fd))))
