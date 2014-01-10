#lang racket
(require rackunit
         rackunit/text-ui
         "../test-helper.rkt"
         (prefix-in b/ "../../src/ch02/bintree/bintree.rkt")
         (prefix-in n/ "../../src/ch02/bintree/node.rkt"))

(define (bintree-test-suite num->tree current
                            move-left move-right
                            insert-left insert-right
                            leaf?)
  (let ([t1 (insert-right
             14
             (insert-left
              12
              (num->tree 13)))])

    (test-suite
     "Examples from book"

     (test-equal?
      "new bintree"
      (current
       (num->tree 13)) 13)

     (test-equal?
      "move left"
      (current (move-left t1)) 12)

     (test-equal?
      "move right"
      (current (move-right t1)) 14)

     (test-true
      "reached leaf"
      (leaf? (move-right (move-left t1))))

     (test-equal?
      "insertion left"
      (current (move-left (insert-left 15 t1))) 15)

     (test-equal?
      "insertion right"
      (current (move-right (insert-right 15 t1))) 15)

     (test-equal?
      "move after insertion right"
      (current (move-left (move-right (insert-right 15 t1)))) 14))))

(run-for-all bintree-test-suite
             ((b/number->bintree
               b/current-element
               b/move-to-left
               b/move-to-right
               b/insert-to-left
               b/insert-to-right
               b/at-leaf?)

              (n/number->bintree
               n/current-element
               n/move-to-left
               n/move-to-right
               n/insert-to-left
               n/insert-to-right
               n/at-leaf?)))

(define node-test-suite
  (let ([t2 (n/insert-to-right
             14
             (n/insert-to-left
              12
              (n/number->bintree 13)))])
    (test-suite
     "Exercise 2.20 tests move-up and at-root?"

     (test-true
      "Recognize the root"
      (n/at-root? t2))

     (test-false
      "left child is not root"
      (n/at-root? (n/move-to-left t2)))

     (test-false
      "right child is not root"
      (n/at-root? (n/move-to-right t2)))

     (test-equal?
      "come back from left excursion"
      (n/current-element
       (n/move-up
        (n/move-to-left t2)))
      (n/current-element t2))

     (test-equal?
      "come back from right excursion"
      (n/current-element
       (n/move-up
        (n/move-to-right t2)))
      (n/current-element t2)))))

(run-tests node-test-suite)
