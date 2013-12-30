#lang racket

(require racket/trace
         rackunit)

;; Exercise 2.1
;; Implement zero, is-zero?, successor, predecessor for bigits
;; Then compute (bigit/factorial 10)

(define bigit/base 16) ;; base 16 as in book, but works with any base

(define (zero)
  '())

(define (is-zero? b)
  (null? b))

;; For internal use or external convenience
;; we convert base 10 numbers to the bigits
(define (bigit/representation base10)
  (if (= 0 base10) (zero)
      (let-values ([(q r) (quotient/remainder base10 bigit/base)])
        (cons r (bigit/representation q)))))

(define (successor b)
  (cond [(is-zero? b) (bigit/representation 1)]
        [(< (add1 (car b)) bigit/base)
         (cons (add1 (car b)) (cdr b))]
        [else
         (cons 0 (successor (cdr b)))]))

(define (predecessor b)
  (cond [(equal? b (bigit/representation 1)) (zero)]
        [(< 0 (car b))
         (cons (sub1 (car b)) (cdr b))]
        [else
         (cons (sub1 bigit/base)
               (predecessor (cdr b)))]))

;; Need addition to do multiplication
(define (bigit/add a b)
  (if (is-zero? a) b
      (bigit/add (predecessor a)
                 (successor b))))

;; Need multiplication to do factorial
(define (bigit/mult a b)
  (cond [(or (is-zero? a)
             (is-zero? b)) (zero)]
        [(equal? (successor (zero)) a) b]
        [else
         (bigit/add b (bigit/mult (predecessor a) b))]))

;; Bigit factorial using accumulator-passing style
(define (bigit/factorial-aps b acc)
  (if (is-zero? b) acc
      (bigit/factorial-aps (predecessor b)
                           (bigit/mult b acc))))

;; Bigit factorial using continuation-passing style
(define (bigit/factorial-cps b k)
  (if (is-zero? b)
      (k (successor (zero)))
      (bigit/factorial-cps (predecessor b)
                           (lambda (v)
                             (k (bigit/mult b v))))))

;; Using CPS for fun
(define (bigit/factorial b)
  ;; or (bigit/factorial-aps b (successor (zero)))
  (bigit/factorial-cps b (lambda (i) i)))

;; Some rack unit tests for good measure
(check-equal? (bigit/representation 258) '(2 0 1) "Right from book")
(check-equal? (successor (successor (zero))) (bigit/representation 2) "Succ")
(check-equal? (successor (predecessor (bigit/representation 16)))
              (bigit/representation 16))
(check-equal? (bigit/add (bigit/representation 17)
                         (bigit/representation 14))
              (bigit/representation 31) "Addition")
(check-equal? (bigit/mult (bigit/representation 12)
                          (bigit/representation 12))
              (bigit/representation 144) "Multiplication")
(check-equal? (bigit/factorial (bigit/representation 5))
              (bigit/representation 120) "Factorial")
