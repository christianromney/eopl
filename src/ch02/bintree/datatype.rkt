#lang eopl

(define-datatype bintree bintree?
  (leaf-node (num integer?))
  (interior-node (key symbol?)
                 (left bintree?)
                 (right bintree?)))

(define (bintree-to-list bt)
  (cases bintree bt
         (leaf-node (num)
                    (list 'leaf-node num))
         (interior-node (k l r)
                        (list 'interior-node k
                              (bintree-to-list l)
                              (bintree-to-list r)))))

(provide leaf-node interior-node bintree-to-list)
