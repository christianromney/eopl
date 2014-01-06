#lang eopl

(define (empty-env)
  (lambda (method)
    (cond [(eqv? method 'apply-env)
           (lambda (s)
             (eopl:error 'apply-env "No binding found for ~s" s))]
          [(eqv? method 'empty-env?)
           (lambda () #t)]
          [(eqv? method 'has-binding?)
           (lambda (s) #f)])))

(define (empty-env? env)
  ((env 'empty-env?)))

(define (has-binding? env s)
  ((env 'has-binding?) s))

(define (extend-env var val env)
  (lambda (method)
    (cond [(eqv? method 'empty-env?)
           (lambda () #f)]
          [(eqv? method 'has-binding?)
           (lambda (s)
             (if (eqv? var s) #t
                 (has-binding? env s)))]
          [(eqv? method 'apply-env)
           (lambda (s)
             (if (eqv? s var) val
                 (apply-env env s)))])))

(define (apply-env env search)
  (if (procedure? env)
      ((env 'apply-env) search)
      (eopl:error "Invalid environment ~s" env)))

(provide empty-env extend-env apply-env empty-env? has-binding?)
