#lang racket

;; 1.2.1
;; (list-length '(a b c)) => 3
(define (list-length lst)
  (if (null? lst)
      0
      (+ 1 (list-length (cdr lst)))))

;; 1.2.2
;; (nth-element '(a b c) 2) => c
(define (nth-element lst n)
  (define (pluralize n sng plu)
    (if (= n 1) sng plu))
  (cond [(null? lst)
         (let ([num (+ n 1)])
           (error 'nth-element "List too short by ~s ~s"
                  num (pluralize num 'element 'elements)))]
        [(= n 0) (car lst)]
        [else
         (nth-element (cdr lst) (- n 1))]))

;; 1.2.3
;; (remove-first 'b '(a b c b a)) => (a c b a)
(define (remove-first s los)
  (cond [(null? los) '()]
        [(eqv? s (car los)) (cdr los)]
        [else
         (cons (car los) (remove-first s (cdr los)))]))

;; 1.2.4 occurs-free?
;; LcExp ::= Identifier | (lambda (Identifier) LcExp) | (LcExp LcExp)
;; Identifier ::= Symbol, where Identifier =/= lambda
(define (bound-var lambda-exp)
    (caadr lambda-exp))

(define (body lambda-exp)
  (caddr lambda-exp))

(define (bound? s lambda-exp)
  (eqv? s (bound-var lambda-exp)))

(define (identifier? s)
  (and (symbol? s)
       (not (eqv? s 'lambda))))

(define (lambda? lcexp)
  (and (list? lcexp)
       (eqv? 'lambda (first lcexp))
       (list? (second lcexp))
       (= 3 (length lcexp))))

(define (application? lcexp)
  (and (list? lcexp)
       (= 2 (length lcexp))))

(define (occurs-free? sym lcexp)
  (cond
   [(identifier? lcexp)
    (eqv? sym lcexp)]
   [(lambda? lcexp)
    (and (not (bound? sym lcexp))
         (occurs-free? sym (body lcexp)))]
   [(application? lcexp)
    (or (occurs-free? sym (first lcexp))
        (occurs-free? sym (second lcexp)))]
   [else
    (error 'occurs-free? "Invalid lambda calculus expression ~s" lcexp)]))

;; 1.2.5 subst
;; (subst 'hi 'bye '(say hola hi bye ((say bye) bye) bye))
;; Slist ::= () | (Sexp . Slist)
;; Sexp  ::= Symbol | Slist
(define (subst new old slist)
  (cond [(null? slist) '()]
        [(list? slist)
         (let ([sexp (car slist)])
           (cons
            (if (symbol? sexp)
                (if (eqv? old sexp) new sexp)
                (subst new old sexp))
            (subst new old (cdr slist))))]
        [else
         (error 'subst "Invalid s-list ~s" slist)]))

;; Exercise write subst by using the original (Kleene star) grammar by using map
;; Slist ::= ({Sexp}*)
;; Sexp  ::= Symbol | Slist
(define (subst-map new old slist)
  (map (lambda (sexp)
         (if (symbol? sexp)
             (if (eqv? old sexp) new sexp)
             (subst-map new old sexp))) slist))

;; 1.3 number-elements
;; (number-elements '(a b c d)) => '((0 a) (1 b) (2 c) (3 d))
(define (number-elements-from lst n)
  (if (null? lst) '()
      (cons
       (list n (car lst))
       (number-elements-from (cdr lst) (+ 1 n)))))

(define (number-elements lst)
  (number-elements-from lst 0))

(define (partial-vector-sum v n)
  (if (zero? n)
      (vector-ref v 0)
      (+ (vector-ref v n)
         (partial-vector-sum v (- n 1)))))

(define (vector-sum v)
  (let ([n (vector-length v)])
    (if (zero? n) 0
        (partial-vector-sum v (- n 1)))))

;; 1.4 Exercises
;; s = Symbol, n = non-negative integer
;; lst = list, los = list of symbols
;; loi = list of integers, slist = s-list
;; x = any scheme value
;; pred = predicate

;; 1.15 duple: Integer x Any -> ListOf(Any)
;; Returns a list containing n copies of x
(define (duple n x)
  (if (zero? n) '()
      (cons x (duple (- n 1) x))))

;; 1.16 invert: ListOf((Any Any)) -> ListOf((Any Any))
(define (invert lst)
  (if (null? lst) '()
      (let ([a (first (car lst))]
            [b (second (car lst))])
        (cons (list b a)
              (invert (cdr lst))))))

;; 1.17 down: List -> List
;; Wrap parentheses around each top-level element in list
;; Is this cheating? We've already been introduced to map, after all...
(define (down lst)
  (map list lst))

;; 1.18 swapper: Symbol x Symbol x List -> List
;; swapper exchanges all instances of symbol1 for symbol2 and vice-versa
;; in the list
(define (swapper a b lst)
  (map (lambda (x)
         (if (list? x)
             (swapper a b x)
             (cond [(eqv? a x) b]
                   [(eqv? b x) a]
                   [else x]))) lst))

;; 1.19 listset: List x Int x Any -> List
;; Replaces the list item at the given index with the supplied value
;; Here's an opportunity to observe the 'no mysterious auxiliaries' rule,
;; but we will do a little 'software engineering' and use letrec so as
;; not to pollute the namespace. We also avoid visiting the elements
;; past the index (an obvious optimization).
(define (listset lst n val)
  (letrec ([listset-from
            (lambda (lst n val cur)
              (cond [(null? lst) '()]
                    [(= cur n)
                     (cons val (cdr lst))]
                    [else
                     (cons (car lst)
                           (listset-from (cdr lst) n val (+ cur 1)))]))])
    (listset-from lst n val 0)))

;; 1.20 count-occurrences: Symbol x SList -> Int
;; Counts the occurrences of the given symbol anywhere in the SList
(define (count-occurrences s slist)
  (letrec ([co-from
            (lambda (s slist acc)
              (cond [(null? slist) acc]
                    [(symbol? (car slist))
                     (co-from s (cdr slist) (if (eqv? s (car slist))
                                                (+ acc 1)
                                                acc))]
                    [else
                     (+ (co-from s (car slist) acc)
                        (co-from s (cdr slist) acc))]))])
    (co-from s slist 0)))

;; 1.21 product: SetOf(Symbol) x SetOf(Symbol) -> SetOf((Symbol Symbol))
;; Cartesian product of two sets of symbols
;; append-map is just (apply append (map f coll))
(define (product sos1 sos2)
  (append-map (lambda (a)
                (map (lambda (b)
                       (list a b)) sos2)) sos1))

;; 1.22 filter-in: (Any -> bool) x List -> List
;; Returns those elements in the list that satisfy the predicate
;; We'll do this one the 'long way' rather than relying on the
;; Racket/Scheme's built-in filter. I'm feeling only *slightly*
;; guilty about product above. ;)
(define (filter-in pred lst)
  (if (null? lst) '()
      (if (pred (car lst))
          (cons (car lst)
                (filter-in pred (cdr lst)))
          (filter-in pred (cdr lst)))))

;; Just for fun, let's do remove-in!
(define (complement f)
  (lambda (x) (not (f x))))

(define (remove-in pred lst)
  (filter-in (complement pred) lst))

;; 1.23 list-index: (Any -> bool) x List -> Int | #f
;; This function returns the index of the first element that satisfies
;; the predicate, or false if none do.
;; This is kind of ugly since the function returns an Int or
;; bool (false). Like one of ML's datatypes...anyway, nothing says
;; our auxiliary function has to follow this specification.
;; We will use -1 for not found and then translate.
(define (list-index pred lst)
  (letrec ([index-of
            (lambda (pred lst cur)
              (cond [(null? lst) -1]
                    [(pred (car lst)) cur]
                    [else
                     (index-of pred (cdr lst) (+ cur 1))]))]
           [is-found
            (lambda (n)
              (if (<= 0 n) n #f))])
    (is-found (index-of pred lst 0))))

;; 1.24 (every? pred lst) returns #t if every element in the list
;; satisfies the given predicate and #f otherwise.
;; every?: (Any -> bool) x List -> bool
;; This is one (clever?), albeit inefficient  way of writing 'every?'.
;; Since 'and' is a macro, we have to wrap its use in a lambda.
;; We first map the predicate across the list and then fold or reduce
;; the resulting list with our function wrapper over 'and'. This works
;; since any #f value will cause 'and' to return #f. It is inefficient
;; because we cannot short-circuit upon encountering the first #f value
;; due to the fact that the predicate is first mapped over the list
;; indiscriminately.
(define (every? pred lst)
  (foldr (lambda (x y)
           (and x y) #t (map pred lst))))

;; 1.25 (exists? pred lst) returns #t if any element in the list
;; satisfies the predicate. This could be accomplished by replacing
;; the 'and' in every? with an 'or', but we will write this function
;; the 'long' way-demonstrating the ability to short-circuit.
;; exists?: (Any -> bool) x List -> bool
(define (exists? pred lst)
  (cond [(null? lst) #f]
        [(pred (car lst)) #t]
        [else
         (exists? pred (cdr lst))]))

;; 1.26 (up lst) removes a pair of parens from each top-level element
;; up: List -> List
;; The approach will be to introduce a tail-recursive procedure which
;; takes an extra accumulator argument representing the structure to
;; be returned. We will append every car of the list onto this structure.
;; We can profit from the fact that append takes two lists to add the
;; elements of the car to the accumulator when the car is a list.
;; If the car is an atom then we wrap it in a list.
(define (up lst)
  (letrec ([to-list
            (lambda (x)
              (if (list? x) x (list x)))]
           [promote
            (lambda (lst acc)
              (if (null? lst) acc
                  (promote (cdr lst) (append acc (to-list (car lst))))))])
    (promote lst '())))

;; 1.27 (flatten slist) returns list of symbols contained in slist
;; in order which they appear (remove all parentheses)
;; flatten: SList -> ListOf(Atom)
(define (flatten slist)
  (cond [(null? slist) '()]
        [(list? (car slist))
         (append (flatten (car slist))
                 (flatten (cdr slist)))]
        [else
         (cons (car slist)
               (flatten (cdr slist)))]))

;; 1.28 (merge loi1 loi2) given two sorted lists of integers,
;; returns a combined sorted list of integers made from the
;; elements of the two argument lists
;; merge: ListOf(Int) x ListOf(Int) -> ListOf(Int)
(define (merge loi1 loi2)
  (cond [(null? loi1) loi2]
        [(null? loi2) loi1]
        [(<= (car loi1) (car loi2))
         (merge (cdr loi1)
                (cons (car loi1) loi2))]
        [else
         (cons (car loi2)
               (merge loi1 (cdr loi2)))]))

;; 1.29 (sort loi) returns the elements of loi in ascending order
;; sort: ListOf(Int) -> ListOf(Int)
;;
;; Might as well do something interesting and reasonably intelligent,
;; so here's mergesort which performs reasonably well and lends itself
;; pretty well to the functional style.
;;
;; The algorithm below is a translation into Scheme of the pseudocode
;; given for the top-down implementation using lists here:
;; http://en.wikipedia.org/wiki/Merge_sort
(define (halves lst)
  (split-at lst (quotient (length lst) 2)))

(define mergesort/sort
  (lambda (pred lst)
    (if (< (length lst) 2) lst
        (let-values [((l r) (halves lst))]
          (mergesort/merge pred
                           (mergesort/sort pred l)
                           (mergesort/sort pred r)
                           '())))))
(define mergesort/result
  (lambda (pred left right acc)
    acc))

(define mergesort/left
  (lambda (pred left right acc)
    (mergesort/merge pred (cdr left) right (append acc (list (car left))))))

(define mergesort/right
  (lambda (pred left right acc)
    (mergesort/merge pred left (cdr right) (append acc (list (car right))))))

(define mergesort/merge
  (lambda (pred left right acc)
    (let ([merge-op
           (cond [(and (null? left) (null? right)) mergesort/result]
                 [(null? right) mergesort/left]
                 [(null? left) mergesort/right]
                 [(= (car left) (car right)) mergesort/left]
                 [(pred (car left) (car right)) mergesort/left]
                 [else mergesort/right])])
      (merge-op pred left right acc))))

(define (sort loi)
  (mergesort/sort < loi))

;; Exercise 1.30 (sort/predicate pred loi)
;; This version of sort allows the specification of the predicate.
;; To accomplish this, I promoted the private auxilliary mergesort functions
;; to full-fledged namespace functions. Next, I added an extra argument
;; to receive the predicate. Lastly, I made (sort loi) use the default
;; predicate '<'.
(define (sort/predicate pred loi)
  (mergesort/sort pred loi))

;; Exercise 1.31 bintree
;; Bintree ::= Int | (Symbol Bintree Bintree)
(define (leaf? bt)
  (number? bt))

(define (leaf n)
  n)

(define (interior-node s l r)
  (list s l r))

(define (lson bt)
  (cadr bt))

(define (rson bt)
  (caddr bt))

(define (contents-of bt)
  (if (leaf? bt) bt
      (car bt)))

;; 1.32 double-tree: Bintree -> Bintree
;; doubles all the leaf node values
(define (double-tree bt)
  (if (leaf? bt)
      (leaf (* 2 (contents-of bt)))
      (interior-node (contents-of bt)
                     (double-tree (lson bt))
                     (double-tree (rson bt)))))

;; 1.33 mark-leaves-with-red-depth: Bintree -> Bintree
;; Takes a bintree and produces a new bintree of the same shape
;; as the original except that each leaf is marked with the
;; number of nodes between it and the root that contain the
;; symbol 'red'
(define (mark-leaves-with-red-depth btree)
  (letrec ([calc
            (lambda (bt depth)
              (if (eqv? (contents-of bt) 'red)
                  (+ depth 1)
                  depth))]
           [mark-leaves-from
            (lambda (bt depth)
              (let ([new-depth (calc bt depth)])
                (if (leaf? bt)
                    (leaf depth)
                    (interior-node (contents-of bt)
                                   (mark-leaves-from (lson bt) new-depth)
                                   (mark-leaves-from (rson bt) new-depth)))))])
    (mark-leaves-from btree 0)))

;; 1.34 path: Int x BinSearchTree -> ListOf(Symbol)
;; path should take an integer, n, and a binary search tree, btree,
;; containing n. It should return a list containing the symbols
;; 'left' and/or 'right' that indicate how to arrive at n.
;; If n is found at the root, it returns the empty list.
;; BTree ::= () | (Int BTree BTree)
(define (path num btree)
  (letrec ([node-value
            (lambda (bt) (car bt))]
           [l-branch
            (lambda (bt) (cadr bt))]
           [r-branch
            (lambda (bt) (caddr bt))]
           [conj
            (lambda (lst v) (append lst (list v)))]
           [search
            (lambda (n bt p)
              (cond [(= n (node-value bt)) p]
                    [(< n (node-value bt))
                     (search n (l-branch bt) (conj p 'left))]
                    [else
                     (search n (r-branch bt) (conj p 'right))]))])
    (search num btree '())))

;; 1.35 number-leaves: Bintree -> Bintree
;; Write a procedure number-leaves which takes a bintree
;; and produces a bintree like the original except that
;; the leaves are numbered from zero.
(define (number-leaves bintree)
  (letrec ([max-val
            (lambda (bt)
              (if (leaf? bt)
                  (contents-of bt)
                  (max-val (rson bt))))]
           [number-from
            (lambda (bt n)
              (if (leaf? bt)
                  (leaf n)
                  (let ([left (number-from (lson bt) n)])
                    (interior-node
                     (contents-of bt)
                     left
                     (number-from (rson bt)
                                  (+ (max-val left) 1))))))])
    (number-from bintree 0)))
