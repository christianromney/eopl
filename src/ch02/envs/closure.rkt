#lang eopl

;; This language cannot provide has-binding? because although
;; it has a means of raising an eopl:error, it has no means
;; for handling one.

(define (empty-env)
  (lambda (search)
    (if (eqv? search 'empty-env/empty-env?) #t
        (eopl:error 'apply-env "No binding found for ~s" search))))

(define (empty-env? env)
  (apply-env env 'empty-env/empty-env?))

(define (extend-env var val env)
  (lambda (search)
    (cond [(eqv? search 'empty-env/empty-env?) #f]
          [(eqv? search var) val]
          [else
           (apply-env env search)])))

(define (apply-env env search)
  (if (procedure? env)
      (env search)
      (eopl:error "Invalid environment ~s" env)))

(provide empty-env extend-env apply-env empty-env?)
