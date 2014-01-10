#lang eopl

;; Internal functions

(define (make-leaf)
  '())

(define (make-bintree n left right up)
  (list n left right up))

(define (number+son->bintree n son)
  (cond [(at-leaf? son)
         (number->bintree n)]
        [(< (current-element son) n)
         (make-bintree n son (make-leaf) (make-leaf))]
        [else
         (make-bintree n (make-leaf) son (make-leaf))]))

(define (link-children self)
  (let ([left  (move-to-left self)]
        [right (move-to-right self)])

    ;; remake self
    (make-bintree
     (current-element self)

     ;; remake left
     (if (at-leaf? left) left

         (make-bintree
          (current-element left)
          (move-to-left left)
          (move-to-right left)
          self))

     ;; remake right
     (if (at-leaf? right) right
         (make-bintree
          (current-element right)
          (move-to-left right)
          (move-to-right right)
          self))

     (move-up self))))

;; Public interface

(define (number->bintree n)
  (make-bintree n
                (make-leaf) ;; left
                (make-leaf) ;; right
                (make-leaf) ;; up
                ))

(define (at-leaf? b)
  (null? b))

(define (at-root? b)
  (if (at-leaf? b) #f
      (null? (move-up b))))

(define (current-element b)
  (car b))

(define (move-to-left b)
  (cadr b))

(define (move-to-right b)
  (caddr b))

(define (move-up b)
  (cadddr b))

(define (insert-to-left n b)
  (let ([old-item (move-to-left b)])
    (link-children
     (make-bintree
      (current-element b)
      (number+son->bintree n old-item)
      (move-to-right b)
      (move-up b)))))

(define (insert-to-right n b)
  (let ([old-item (move-to-right b)])
    (link-children
     (make-bintree
      (current-element b)
      (move-to-left b)
      (number+son->bintree n old-item)
      (move-up b)))))

(provide number->bintree
         current-element
         move-to-left
         move-to-right
         at-leaf?
         insert-to-left
         insert-to-right
         move-up
         at-root?)
