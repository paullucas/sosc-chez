(import (rnrs) (mk-r6rs))

(define sosc-src
  '("../src/bytevector.scm"
    "../src/sosc.scm"
    "../src/transport.scm"))

(mk-r6rs '(sosc)
         (cons "../src/ip.ikarus.scm" sosc-src)
         (string-append (list-ref (command-line) 1) "/sosc.ikarus.sls")
         '((rnrs) (rhs) (prefix (ikarus) ikarus:))
         '()
         '())

(mk-r6rs '(sosc)
         (cons "../src/ip.guile.scm" sosc-src)
         (string-append (list-ref (command-line) 1) "/sosc.guile.sls")
         '((rnrs) (rhs))
         '()
         '())

(mk-r6rs '(sosc)
         (cons "../src/ip.mzscheme.scm" sosc-src)
         (string-append (list-ref (command-line) 1) "/sosc.mzscheme.sls")
         '((rnrs) (rhs) (prefix (scheme) plt:))
         '()
         '())

(exit)
