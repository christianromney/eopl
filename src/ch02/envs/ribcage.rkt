#lang eopl

;; Exercise 2.11 Ribcage representation

(define (empty-env) '())

(define (empty-env? e)
  (null? e))

(define (extend-env* vars vals saved)
  (cons (cons vars vals) saved))

(define (extend-env var val saved)
  (cons (cons (list var) (list val)) saved))

(define (has-binding? env s)
  (cond [(empty-env? env) #f]
        [(memv s (envars env)) #t]
        [else
         (has-binding? (previous env) s)]))

(define (envars env)
  (caar env))

(define (envals env)
  (cdar env))

(define (previous env)
  (cdr env))

(define not-found -1)

(define (index-of e lst)
  (letrec ([index-from
            (lambda (e lst pos)
              (cond [(null? lst) -1]
                    [(eqv? e (car lst)) pos]
                    [else
                     (index-from e (cdr lst) (+ 1 pos))]))])
    (index-from e lst 0)))

(define (element-at n lst)
  (letrec ([elem-from
            (lambda (n lst pos)
              (if (= n pos)
                  (car lst)
                  (elem-from n (cdr lst) (+ 1 pos))))])
    (elem-from n lst 0)))

(define (apply-env env s)
  (let ([index (index-of s (envars env))])
    (cond [(empty-env? env)
           (eopl:error 'apply-env "No binding found for ~s" s)]
          [(< not-found index)
           (element-at index (envals env))]
          [else
           (apply-env (previous env) s)])))

(provide empty-env extend-env apply-env empty-env? has-binding? extend-env*)
