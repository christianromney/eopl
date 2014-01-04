#lang eopl

;; Exercise 2.3
;; Define a representation of all the integers (negative and nonnegative)
;; as diff-trees, where a diff-tree is a list defined by the grammar:
;;
;;     Diff-tree ::= (one) | (diff Diff-tree Diff-tree)
;;
(define (one)
  '(one))

(define (sub t1 t2)
  (list 'diff t1 t2))

(define (predecessor t)
  (sub t (one)))

(define (zero)
  (predecessor (one)))

(define (negative-one)
  (predecessor (zero)))

(define (is-zero? t)
  (= 0 (diff-tree->base10 t)))

(define (successor t)
  (sub t (negative-one)))

(define (negative t)
  (sub (zero) t))

(define (plus t1 t2)
  (sub t1 (negative t2)))

;; The conversion functions are O(n) *not* O(1)
(define (diff-tree->base10 t)
  (letrec ([convert
            (lambda (t)
              (cond [(null? t) '()]
                    [(and (list? t)
                          (eqv? (car t) 'diff))
                     (list '-
                           (diff-tree->base10 (cadr t))
                           (diff-tree->base10 (caddr t)))]
                    [(and (list? t)
                          (eqv? (car t) 'one)) 1]
                    [else
                     (eopl:error 'diff-tree->base10 "Invalid diff-tree ~s" t)]))])
    (eval (convert t))))

(define (base10->diff-tree n)
  (letrec ([negative-n
            (lambda (n acc)
              (if (= n 0) acc
                  (negative-n (- n 1)
                              (predecessor acc))))])
    (cond [(= 0 n) (zero)]
          [(< 0 n) (sub (zero)
                        (negative-n n (zero)))]
          [else
           (negative-n (* -1 n) (zero))])))

(provide base10->diff-tree diff-tree->base10 zero is-zero? predecessor successor plus)
