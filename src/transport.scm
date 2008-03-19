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
