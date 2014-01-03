#lang racket
(require rackunit
         "../../src/ch02/bigit/bigit.rkt")

;; Basic example from book
(check-equal? (base10->bigit 258) '(2 0 1) "Right from book")

;; Roundtripping
(check-equal? (bigit->base10 (base10->bigit 1025)) 1025 "Roundtrip representations")

;; Successor vs. Direct representation
(check-equal? (successor (successor (zero))) (base10->bigit 2) "Succ")

;; ((compose Successor  Predecessor) b) == b
(check-equal? (successor (predecessor (base10->bigit 16)))
              (base10->bigit 16) "Successor + Predecessor")

(check-equal? (add (base10->bigit 17)
                   (base10->bigit 14))
              (base10->bigit 31) "Addition")

(check-equal? (mult (base10->bigit 12)
                    (base10->bigit 12))
              (base10->bigit 144) "Multiplication")

(check-equal? (! (base10->bigit 5))
              (base10->bigit 120) "Factorial")
