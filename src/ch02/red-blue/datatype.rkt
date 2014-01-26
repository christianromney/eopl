#lang eopl

(define (every? pred lst)
  (cond [(null? lst) #t]
        [(pred (car lst))
         (every? pred (cdr lst))]
        [else #f]))

(define-datatype red-blue-tree red-blue-tree?
  (red-node (left red-blue-tree?)
            (right red-blue-tree?))
  (blue-node (nodes (lambda (b)
                      (or (null? b)
                          (and (list? b)
                               (every? red-blue-tree? b))))))
  (leaf-node (num integer?)))

;; Exercise 2.26
(define (leaves->red-ancestor-count tree)
  (letrec ([update-red-count
            (lambda (rbt acc)
              (cases red-blue-tree rbt
                     (red-node (left right)
                               (red-node (update-red-count left (+ 1 acc))
                                         (update-red-count right (+ 1 acc))))
                     (blue-node (nodes)
                                (blue-node
                                 (map (lambda (t)
                                        (update-red-count t acc)) nodes)))
                     (leaf-node (_)
                                (leaf-node acc))))])
    (update-red-count tree 0)))

(define (tree->list tree)
  (cases red-blue-tree tree
         (red-node (left right)
                   (list 'red-node
                         (tree->list left)
                         (tree->list right)))
         (blue-node (nodes)
                    (cons 'blue-node
                          (map tree->list nodes)))
         (leaf-node (num)
                    (list 'leaf-node num))))

(provide red-node blue-node leaf-node
         red-blue-tree? leaves->red-ancestor-count
         tree->list)
