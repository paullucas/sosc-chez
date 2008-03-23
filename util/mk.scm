(import (rhs r6rs rhs))
(load "/home/rohan/sw/rhs/util/util.scm")

(mk-r6rs '(sosc r6rs sosc)
	 '("/home/rohan/sw/sosc/src/sosc.scm")
	 "/home/rohan/sw/sosc/r6rs/sosc.sls"
	 '((rnrs base)
	   (rnrs bytevectors)
	   (rnrs io ports)
	   (rnrs io simple)
	   (rhs r6rs rhs))
	 '()
	 '())

(mk-r6rs '(sosc r6rs transport)
	 '("/home/rohan/sw/sosc/ikarus/ip.scm"
	   "/home/rohan/sw/sosc/src/transport.scm")
	 "/home/rohan/sw/sosc/r6rs/transport.ikarus.sls"
	 '((rnrs base)
	   (rnrs records syntactic)
	   (rnrs bytevectors)
	   (rnrs io ports)
	   (rnrs io simple)
	   (rhs r6rs rhs)
	   (sosc r6rs sosc)
	   (only (ikarus) 
		 parameterize 
		 tcp-connect
		 udp-open udp-connect udp-send udp-recv udp-close))
	 '()
	 '())
