(import (rnrs)
        (mk-r6rs))

(mk-r6rs '(sosc)
         '("../src/ikarus/ip.scm"
           "../src/bytevector.scm"
           "../src/sosc.scm"
           "../src/transport.scm")
         (string-append (list-ref (command-line) 1)
                        "/sosc.ikarus.sls")
         '((rnrs)
           (rhs)
           (prefix (ikarus) ikarus:))
         '()
         '())

(exit)
