#lang eopl

(provide number->bintree
         current-element
         move-to-left-son
         move-to-right-son
         at-leaf?
         insert-to-left
         insert-to-right)

;; Internal functions

(define (make-leaf)
  '())

(define (make-bintree n left right)
  (list n left right))

(define (number+son->bintree n son)
  (cond [(at-leaf? son)
         (number->bintree n)]
        [(< (current-element son) n)
         (make-bintree n son (make-leaf))]
        [else
         (make-bintree n (make-leaf) son)]))

;; Public interface

(define (number->bintree n)
  (make-bintree n
                (make-leaf)
                (make-leaf)))

(define (at-leaf? b)
  (null? b))

(define (current-element b)
  (car b))

(define (move-to-left-son b)
  (cadr b))

(define (move-to-right-son b)
  (caddr b))

(define (insert-to-left n b)
  (let ([old-item (move-to-left-son b)])
    (make-bintree
     (current-element b)
     (number+son->bintree n old-item)
     (move-to-right-son b))))

(define (insert-to-right n b)
  (let ([old-item (move-to-right-son b)])
    (make-bintree
     (current-element b)
     (move-to-left-son b)
     (number+son->bintree n old-item))))
