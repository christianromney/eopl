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

(define (leaf-sum bt)
  (cases bintree bt
         (leaf-node (num) num)
         (interior-node (_ left right)
                        (+ (leaf-sum left)
                           (leaf-sum right)))))

(define (node-key bt)
  (cases bintree bt
         (interior-node (key l r) key)
         (leaf-node (_)
                    (eopl:error 'node-key "Leaf node has no key"))))

(define (leaf-node? bt)
  (cases bintree bt
         (leaf-node (_) #t)
         (else #f)))

(define (interior-node? bt)
  (cases bintree bt
         (interior-node (_ l r) #t)
         (else #f)))

(define (max-interior bt)
  (cases bintree bt
         (leaf-node
          (_)
          (eopl:error 'max-interior "Must be passed an interior-node"))

         (interior-node
          (this left right)
          (let* ([l (leaf-sum left)]
                 [r (leaf-sum right)]
                 [b (leaf-sum bt)])
            (cond [(and (leaf-node? left)
                        (leaf-node? right))
                   (eopl:error 'max-interior "Must have at least 1 interior-node")]
                  [(< l r)
                   (if (< b r)
                       (node-key right) this)]
                  [else
                   (if (< b l)
                       (node-key left) this)])))))

(provide leaf-node leaf-sum interior-node bintree-to-list max-interior)
