(module r6rs scheme/base
(provide (all-defined-out))
(define bytevector-ieee-single-ref floating-point-bytes->real)
(define bytevector-ieee-double-ref floating-point-bytes->real)
(define (bytevector-ieee-single-set! v k x e) (real->floating-point-bytes x 4 e v))
(define (bytevector-ieee-double-set! v k x e) (real->floating-point-bytes x 8 e v))
)
