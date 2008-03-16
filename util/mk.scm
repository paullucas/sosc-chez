(load "/home/rohan/sw/rhs/util/util.scm")

(mk-r6rs '(sosc ikarus sosc)
	 '("/home/rohan/sw/sosc/ikarus/ip.scm"
	   "/home/rohan/sw/sosc/src/sosc.scm")
	 "/home/rohan/sw/sosc/ikarus/sosc.scm"
	 '((rnrs base)
	   (rnrs records syntactic)
	   (rnrs bytevectors)
	   (rnrs io ports)
	   (rnrs io simple)
	   (rhs r6rs rhs)
	   (only (ikarus) parameterize tcp-connect))
	 '()
	 '())
