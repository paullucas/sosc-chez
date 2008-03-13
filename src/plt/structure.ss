(module structure scheme/base
(provide define-structure)
(define-syntax define-structure
  (syntax-rules ()
    ((_ name field ...)
     (define-struct name (field ...) #:mutable #:inspector (make-inspector)))))
)
