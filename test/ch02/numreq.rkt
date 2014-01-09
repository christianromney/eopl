#lang racket
(require rackunit
         rackunit/text-ui
         "../../src/ch02/numseq/numseq.rkt")

(define numseq-test-suite
  (test-suite
   "number sequence test suite from book examples"

   (test-equal? "Representation definition"
                (number->sequence 7)
                '(7 () ()))

   (test-equal? "Current element"
                (current-element '(6 (5 4 3 2 1) (7 8 9))) 6)


   (test-equal? "Move left"
                (move-to-left '(6 (5 4 3 2 1) (7 8 9)))
                '(5 (4 3 2 1) (6 7 8 9)))

   (test-equal? "Move right"
                (move-to-right '(6 (5 4 3 2 1) (7 8 9)))
                '(7 (6 5 4 3 2 1) (8 9)))

   (test-equal? "insert to left"
                (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9)))
                '(6 (13 5 4 3 2 1) (7 8 9)))

   (test-equal? "insert to right"
                (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9)))
                '(6 (5 4 3 2 1) (13 7 8 9)))

   (test-true "recognize left end"
              (at-left-end? '(7 () (8 9 10))))

   (test-true "recognize right end"
              (at-right-end? '(7 (1 2 3) ())))

   (test-true "recognize left end"
              (at-left-end? '(7 () ())))

   (test-true "recognize right end"
              (at-left-end? '(7 () ())))

   (test-false "recognize left end"
              (at-right-end? '(7 () (8 9 10))))

   (test-false "recognize right end"
               (at-left-end? '(7 (1 2 3) ())))

   (test-exn "Can't move left of left end"
             exn?
             (lambda ()
               (move-to-left '(7 () ()))))

   (test-exn "Can't move right of right end"
             exn?
             (lambda ()
               (move-to-right '(7 () ()))))))

(run-tests numseq-test-suite)
