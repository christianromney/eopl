#lang eopl

(define (any? x) #t)

(define-datatype env env?
  (empty-env)
  (extend-env [var symbol?]
              [val any?]
              [saved env?]))

(define (apply-env e search)
  (cases env e
         (empty-env ()
                    (eopl:error 'apply-env "No binding found for ~s" search))
         (extend-env (var val saved)
                     (if (eqv? var search) val
                         (apply-env saved search)))
         (else
          (eopl:error 'apply-env "Invalid environment"))))

(provide empty-env extend-env apply-env)
