;; any -> bool
(define udp:socket?
  #f)

;; string -> int -> socket
(define udp:open 
  (lambda (h p)
    #f))

;; socket -> bytevector -> ()
(define udp:send 
  (lambda (u b)
    #f))

;; socket -> maybe bytevector
(define udp:recv
  (lambda (u)
    #f))

;; socket -> ()
(define udp:close 
  (lambda (u)
    #f))

(define-record-type tcp* (fields i o h p))

;; any -> bool
(define tcp:socket?
  tcp*?)

;; string -> int -> socket
(define tcp:open 
  (lambda (h p)
    (let-values
     (((o i) (tcp-connect h p)))
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
    (close-port (tcp*-i fd))
    (close-port (tcp*-o fd))))
