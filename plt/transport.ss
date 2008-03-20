(module transport rhs/plt/empty
(provide send recv
	 (all-from-out "ip.ss"))
(require rnrs/base-6
	 rnrs/bytevectors-6
	 rnrs/records/syntactic-6
	 rnrs/io/simple-6
	 (except-in rnrs/io/ports-6
		    current-output-port
		    current-input-port
		    current-error-port))
(require rhs/r6rs/rhs)
(require "ip.ss")
(require "sosc.ss")
(require mzlib/include)
(include "../src/transport.scm")
)
