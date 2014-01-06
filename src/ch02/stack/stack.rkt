#lang eopl

;; Exercise 2.12 (2.4) Stack interface definition
;;
;; (empty-stack)          = ⎡∅⎤
;; (empty-stack? ⎡s⎤)     = #t if ⎡s⎤ = ⎡∅⎤
;;                          #f otherwise
;; (push v ⎡s⎤)           = ⎡t⎤
;; (pop ⎡t⎤)              = ⎡s⎤
;; (top ⎡t⎤)              = v

(define (empty-stack)
  (lambda (msg)
    (cond [(eqv? msg 'empty-stack?)
           (lambda () #t)]
          [(eqv? msg 'top)
           (eopl:error 'top "Called top on empty stack")]
          [(eqv? msg 'pop)
           (eopl:error 'pop "Called pop on empty stack")]
          [(eqv? msg 'push)
           (lambda (v st)
             (extend-stack v st))])))

(define (extend-stack v st)
  (lambda (msg)
    (cond [(eqv? msg 'empty-stack?)
           (lambda () #f)]
          [(eqv? msg 'top)
           (lambda () v)]
          [(eqv? msg 'pop)
           (lambda () st)]
          [(eqv? msg 'push)
           (lambda (v st)
             (extend-stack v st))])))

(define (empty-stack? st)
  ((st 'empty-stack?)))

(define (push v st)
  ((st 'push) v st))

(define (pop st)
  ((st 'pop)))

(define (top st)
  ((st 'top)))

(provide empty-stack empty-stack? push pop top)
