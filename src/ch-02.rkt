#lang racket

(require racket/trace
         rackunit)

;; Exercise 2.1
;; Implement zero, is-zero?, successor, predecessor for bigits
;; Then compute (bigit/factorial 10)
(define bigit/base 16)

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
