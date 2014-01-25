#lang racket
(require rackunit
         rackunit/text-ui
         "../test-helper.rkt"
         (prefix-in b/ "../../src/ch02/bintree/bintree.rkt")
         (prefix-in p/ "../../src/ch02/bintree/bintree-parent.rkt")
         (prefix-in d/ "../../src/ch02/bintree/datatype.rkt"))

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

              (p/number->bintree
               p/current-element
               p/move-to-left
               p/move-to-right
               p/insert-to-left
               p/insert-to-right
               p/at-leaf?)))

(define node-test-suite
  (let ([t2 (p/insert-to-right
             14
             (p/insert-to-left
              12
              (p/number->bintree 13)))])
    (test-suite
     "Exercise 2.20 tests move-up and at-root?"

     (test-true
      "Recognize the root"
      (p/at-root? t2))

     (test-false
      "left child is not root"
      (p/at-root? (p/move-to-left t2)))

     (test-false
      "right child is not root"
      (p/at-root? (p/move-to-right t2)))

     (test-equal?
      "come back from left excursion"
      (p/current-element
       (p/move-up
        (p/move-to-left t2)))
      (p/current-element t2))

     (test-equal?
      "come back from right excursion"
      (p/current-element
       (p/move-up
        (p/move-to-right t2)))
      (p/current-element t2)))))

(run-tests node-test-suite)


(define (bt-datatype-suite leaf interior bintree->list max)
  (let* ([bt (interior 'a (leaf 3) (leaf 4))]
         [tree-1 (interior 'foo (leaf 2) (leaf 3))]
         [tree-2 (interior 'bar (leaf -1) tree-1)]
         [tree-3 (interior 'baz tree-2 (leaf 1))])

    (test-suite
     "Test the exercises from the end of chapter 2"

     (test-equal?
      "book sample for bintree-to-list"

      (bintree->list bt)
      '(interior-node a (leaf-node 3) (leaf-node 4)))

     (test-equal?
      "first sample for max-interior"
      (max tree-2) 'foo)

     (test-equal?
      "second sample for max-interior"
      (max tree-3) 'baz))))

(run-for-all bt-datatype-suite
             ((d/leaf-node d/interior-node d/bintree-to-list d/max-interior)))
