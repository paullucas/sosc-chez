;; data udp
(define-struct udp* (s h p))

;; any -> bool
(define udp:socket?
  udp*?)

;; string -> int -> socket
(define udp:open 
  (lambda (h p)
    (make-udp* (udp-open-socket) h p)))

;; socket -> bytevector -> ()
(define udp:send 
  (lambda (u b)
    (let ((s (udp*-s u))
	  (h (udp*-h u))
	  (p (udp*-p u)))
      (udp-send-to* s h p b))))

;; socket -> maybe bytevector
(define udp:recv
  (lambda (u)
    (let* ((s (udp*-s u))
	   (h (udp*-h u))
	   (p (udp*-p u))
	   (b (make-bytes 8192))
	   (r (sync/timeout 1.0 (udp-receive!-evt s b))))
      (if r
	  (subbytes b 0 (car r))
	  #f))))

;; socket -> ()
(define udp:close 
  (lambda (u)
    (udp-close (udp*-s u))))

;; data tcp
(define-struct tcp* (i o h p))

;; any -> bool
(define tcp:socket?
  tcp*?)

;; string -> int -> socket
(define tcp:open 
  (lambda (h p)
    (let-values
     (((i o) (tcp-connect h p)))
     (file-stream-buffer-mode i 'none)
     (file-stream-buffer-mode o 'none)
     (make-tcp* i o h p))))

;; socket -> bytevector -> ()
(define tcp:send
  (lambda (fd b)
    (let ((o (tcp*-o fd)))
      (put-bytevector o b))))
  
;; socket -> int -> maybe bytevector
(define tcp:read
  (lambda (fd n)
    (get-bytevector-n (tcp*-i fd) n)))

;; socket -> ()
(define tcp:close
  (lambda (fd)
    (close-input-port (tcp*-i fd))
    (close-output-port (tcp*-o fd))))