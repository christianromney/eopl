#lang racket

(define bigit/base 16) ;; base 16 as in book, but works with any base

;; *** Integer Interface ***
(define (zero)
  '())

(define (is-zero? b)
  (null? b))

(define (successor b)
  (cond [(is-zero? b) (base10->bigit 1)]
        [(< (add1 (car b)) bigit/base)
         (cons (add1 (car b)) (cdr b))]
        [else
         (cons 0 (successor (cdr b)))]))

(define (predecessor b)
  (cond [(equal? b (base10->bigit 1)) (zero)]
        [(< 0 (car b))
         (cons (sub1 (car b)) (cdr b))]
        [else
         (cons (sub1 bigit/base)
               (predecessor (cdr b)))]))


;; **** Conversion Functions **** ;;
(define (base10->bigit base10)
  (if (= 0 base10) (zero)
      (let-values ([(q r) (quotient/remainder base10 bigit/base)])
        (cons r (base10->bigit q)))))


(define (bigit->base10 bigit)
  (letrec ([calc
            (lambda (b place acc)
              (if (null? b)
                  acc
                  (calc (cdr b)
                        (add1 place)
                        (+ acc (* (car b) (expt bigit/base place))))))])
    (calc bigit 0 0)))

;; Need addition to do multiplication
(define (add a b)
  (if (is-zero? a) b
      (add (predecessor a)
           (successor b))))

;; Need multiplication to do factorial
(define (mult a b)
  (cond [(or (is-zero? a)
             (is-zero? b)) (zero)]
        [(equal? (successor (zero)) a) b]
        [else
         (add b (mult (predecessor a) b))]))

;; Bigit factorial using accumulator-passing style
(define (! b)
  (letrec ([!-aps
            (lambda (b acc)
              (if (is-zero? b) acc
                  (!-aps (predecessor b)
                         (mult b acc))))])
    (!-aps b (successor (zero)))))

;; Module exports
(provide is-zero? zero successor predecessor bigit->base10 base10->bigit ! add mult)
