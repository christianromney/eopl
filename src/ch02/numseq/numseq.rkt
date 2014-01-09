#lang eopl


(define (empty-seq)
  '())

(define (prev-list s)
  (cadr s))

(define (next-list s)
  (caddr s))

(define (add-to-list n l)
  (cons n l))

(define (make-numseq n left right)
  (list n left right))

(define (number->sequence n)
  (make-numseq n
        (empty-seq)
        (empty-seq)))

(define (current-element s)
  (car s))

(define (move-to-left s)
  (if (at-left-end? s)
      (eopl:error 'move-to-left "Can't move left of left end")
      (make-numseq
       (car (prev-list s))
       (cdr (prev-list s))
       (add-to-list
        (current-element s)
        (next-list s)))))

(define (move-to-right s)
  (if (at-right-end? s)
      (eopl:error 'move-to-right "Can't move right of right end")
      (make-numseq
       (car (next-list s))
       (add-to-list
        (current-element s)
        (prev-list s))
       (cdr (next-list s)))))

(define (insert-to-left n s)
  (make-numseq
   (current-element s)
   (add-to-list n (prev-list s))
   (next-list s)))

(define (insert-to-right n s)
  (make-numseq
   (current-element s)
   (prev-list s)
   (add-to-list n (next-list s))))

(define (at-left-end? s)
  (null? (prev-list s)))

(define (at-right-end? s)
  (null? (next-list s)))

(provide number->sequence current-element
         move-to-left move-to-right
         insert-to-left insert-to-right
         at-left-end?
         at-right-end?)
