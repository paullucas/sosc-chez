(module sosc scheme/base
(require mzlib/include)
(require (prefix-in r6rs: rnrs/bytevectors-6))
(require (prefix-in r6rs: rnrs/io/ports-6))
(require (prefix-in r6rs: rsc3/mzscheme/r6rs))
(require sosc/src/plt/r6rs)
(require sosc/src/plt/structure)
(require sosc/src/plt/udp)
(require rhs/plt/rhs)
(provide (all-defined-out)
	 (all-from-out sosc/src/plt/structure
		       sosc/src/plt/udp))
(include "src/sosc.scm")
)
