#lang eopl

;; This is the first implementation given in the book
(define (empty-env) '(empty-env))

(define (empty-env? env)
  (and (list? env)
       (= 1 (length env))
       (eqv? 'empty-env (car env))))

(define (extend-env var val env)
  (list 'extend-env var val env))

(define (apply-env env search)
  (cond [(eqv? 'empty-env (car env))
         (eopl:error 'apply-env "No binding found for ~s" search)]
        [(eqv? 'extend-env (car env))
         (if (eqv? (cadr env) search)
             (caddr env)
             (apply-env (cadddr env) search))]
        [else
         (eopl:error 'apply-env "Invalid environment ~s" env)]))

(define (has-binding? env s)
  (cond [(eqv? 'empty-env (car env)) #f]
        [(eqv? 'extend-env (car env))
         (if (eqv? (cadr env) s) #t
             (has-binding? (cadddr env) s))]
        [else
         (eopl:error 'has-binding? "Invalid environment ~s" env)]))

(provide empty-env extend-env apply-env empty-env? has-binding?)
