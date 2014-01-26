#lang racket
(require rackunit
         rackunit/text-ui
         "../test-helper.rkt"
         (prefix-in d/ "../../src/ch02/red-blue/datatype.rkt"))

(define (renumber-leaves-test red blue leaf renumber tree->list)
  (let ([blue-tree (blue (list
                          (leaf 0)
                          (red
                           (red
                            (leaf 1)
                            (leaf 2))
                           (blue '()))
                          (leaf 3)))])
    (test-suite
     "red-blue tree tests"

     (test-equal?
      "leaf-node construction"
      (tree->list (leaf 3))
      '(leaf-node 3))

     (test-equal?
      "red-node construction"
      (tree->list (red (leaf 0) (leaf 1)))
      '(red-node (leaf-node 0) (leaf-node 1)))

     (test-equal?
      "blue-node construction - empty node list"
      (tree->list (blue '()))
      '(blue-node))

     (test-equal?
      "blue-node construction - single node"
      (tree->list (blue (list (leaf 0))))
      '(blue-node (leaf-node 0)))

     (test-equal?
      "blue-node construction - multiple nodes"
      (tree->list blue-tree)
      '(blue-node (leaf-node 0)
                  (red-node
                   (red-node
                    (leaf-node 1)
                    (leaf-node 2))
                   (blue-node))
                  (leaf-node 3)))

     (test-equal?
      "renumbering of red-depth"
      (tree->list (renumber blue-tree))
      '(blue-node (leaf-node 0)
                  (red-node
                   (red-node
                    (leaf-node 2)
                    (leaf-node 2))
                   (blue-node))
                  (leaf-node 0))))))

(run-for-all renumber-leaves-test
             ((d/red-node
               d/blue-node
               d/leaf-node
               d/leaves->red-ancestor-count
               d/tree->list)))
