(module r6rs scheme/base
(provide (all-defined-out))

;; missing from plt rnrs/bytevectors

(define bytevector-ieee-single-ref 
  (lambda (b k e)
    (floating-point-bytes->real b e)))

(define bytevector-ieee-double-ref 
  (lambda (b k e)
    (floating-point-bytes->real b e)))

(define bytevector-ieee-single-set! 
  (lambda (v k x e) 
    (real->floating-point-bytes x 4 e v)))

(define bytevector-ieee-double-set! 
  (lambda (v k x e)
    (real->floating-point-bytes x 8 e v)))

)
