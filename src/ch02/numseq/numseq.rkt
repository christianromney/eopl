#lang eopl

;; Private implementation functions

(define (empty-seq)
  '())

(define (prev-list s)
  (cadr s))

(define (next-list s)
  (caddr s))

(define (next-item-left s)
  (car (prev-list s)))

(define (next-item-right s)
  (car (next-list s)))

(define (shrink-list l)
  (cdr l))

(define (shrink-prev-list s)
  (shrink-list (prev-list s)))

(define (shrink-next-list s)
  (shrink-list (next-list s)))

(define (grow-list n l)
  (cons n l))

(define (grow-next-list n s)
  (grow-list n (next-list s)))

(define (grow-prev-list n s)
  (grow-list n (prev-list s)))

(define (make-numseq n left right)
  (list n left right))

;; Public interface

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
       (next-item-left s)
       (shrink-prev-list s)
       (grow-next-list
        (current-element s) s))))

(define (move-to-right s)
  (if (at-right-end? s)
      (eopl:error 'move-to-right "Can't move right of right end")
      (make-numseq
       (next-item-right s)
       (grow-prev-list
        (current-element s) s)
       (shrink-next-list  s))))

(define (insert-to-left n s)
  (make-numseq
   (current-element s)
   (grow-prev-list n s)
   (next-list s)))

(define (insert-to-right n s)
  (make-numseq
   (current-element s)
   (prev-list s)
   (grow-next-list n s)))

(define (at-left-end? s)
  (null? (prev-list s)))

(define (at-right-end? s)
  (null? (next-list s)))

(provide number->sequence current-element
         move-to-left move-to-right
         insert-to-left insert-to-right
         at-left-end?
         at-right-end?)
