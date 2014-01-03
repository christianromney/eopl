#lang eopl

(define (empty-env)
  (lambda (search)
    (eopl:error 'apply-env "No binding found for ~s" search)))

(define (extend-env var val env)
  (lambda (search)
    (if (eqv? search var)
        val
        (apply-env env search))))

(define (apply-env env search)
  (env search))

(provide empty-env extend-env apply-env)