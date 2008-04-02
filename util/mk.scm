(import (rhs r6rs rhs))
(load "/home/rohan/sw/rhs/util/util.scm")

(mk-r6rs '(sosc r6rs sosc)
	 '("/home/rohan/sw/sosc/src/bytevector.scm"
	   "/home/rohan/sw/sosc/src/sosc.scm")
	 "/home/rohan/sw/sosc/r6rs/sosc.sls"
	 '((rnrs)
           (rhs r6rs rhs))
	 '()
	 '())

(mk-r6rs '(sosc r6rs transport)
	 '("/home/rohan/sw/sosc/ikarus/ip.scm"
	   "/home/rohan/sw/sosc/src/transport.scm")
	 "/home/rohan/sw/sosc/r6rs/transport.ikarus.sls"
	 '((rnrs)
	   (rhs r6rs rhs)
	   (sosc r6rs sosc)
	   (prefix (ikarus) ikarus:))
	 '()
	 '())
