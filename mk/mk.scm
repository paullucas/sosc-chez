(import (rnrs)
        (mk-r6rs))

(mk-r6rs '(sosc)
	 '("/home/rohan/sw/sosc/src/ikarus/ip.scm"
           "/home/rohan/sw/sosc/src/bytevector.scm"
	   "/home/rohan/sw/sosc/src/sosc.scm"
	   "/home/rohan/sw/sosc/src/transport.scm")
	 (string-append (list-ref (command-line) 1)
                        "/sosc.ikarus.sls")
	 '((rnrs)
           (rhs)
           (prefix (ikarus) ikarus:))
	 '()
	 '())

(exit)
