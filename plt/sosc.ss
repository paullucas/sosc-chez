(module sosc rhs/plt/empty
(provide (all-defined-out)
	 (all-from-out "ip.ss"))
(require rhs/r6rs/rhs)
(require rnrs/base-6
	 rnrs/bytevectors-6
	 rnrs/records/syntactic-6
	 rnrs/io/simple-6
	 (except-in rnrs/io/ports-6
		    current-output-port
		    current-input-port
		    current-error-port))
(require "ip.ss")
(require "r6rs.ss")
(require mzlib/include)
(include "../src/sosc.scm")
)
