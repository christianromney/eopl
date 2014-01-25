#lang eopl

;; Exercise 2.22
;;
;; (empty-stack)          = ⎡∅⎤
;; (empty-stack? ⎡s⎤)     = #t if ⎡s⎤ = ⎡∅⎤
;;                          #f otherwise
;; (push v ⎡s⎤)           = ⎡t⎤
;; (pop ⎡t⎤)              = ⎡s⎤
;; (top ⎡t⎤)              = v

(provide stack? empty-stack empty-stack? push pop top)

(define-datatype stack stack?
  (empty-stack)
  (extend-stack (val (lambda (_) #t))
                (s stack?)))

(define (empty-stack? s)
  (cases stack s
         (empty-stack () #t)
         (else #f)))

(define (push v st)
  (extend-stack v st))

(define (pop st)
  (cases stack st
         (empty-stack ()
                      (eopl:error 'pop "Called pop on empty stack"))
         (extend-stack (v st) st)))

(define (top st)
  (cases stack st
         (empty-stack ()
                      (eopl:error 'top "Called top on empty stack"))
         (extend-stack (v st) v)))
