#lang eopl

;; This is the first implementation given in the book
(define (empty-env) '(empty-env))

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

(provide empty-env extend-env apply-env)
