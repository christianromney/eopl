#lang eopl

;; Exercise 2.5 a-list representation of environments
(define (empty-env)
  '())

(define (empty-env? env)
  (null? env))

(define (extend-env var val env)
  (cons (cons var val) env))

(define (apply-env env search)
  (cond [(null? env)
         (eopl:error 'apply-env "No binding found for ~s" search)]
        [(not (and (pair? env)
                   (pair? (car env))))
         (eopl:error 'apply-env "Invalid environment ~s" env)]
        [(eqv? (caar env) search) (cdar env)]
        [else
         (apply-env (cdr env) search)]))

(provide empty-env extend-env apply-env empty-env?)
