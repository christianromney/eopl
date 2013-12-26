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
  (cond ((null? lst)
         (let ((num (+ n 1)))
           (error 'nth-element "List too short by ~s ~s"
                  num (pluralize num 'element 'elements))))
        ((= n 0) (car lst))
        (else
         (nth-element (cdr lst) (- n 1)))))

;; 1.2.3
;; (remove-first 'b '(a b c b a)) => (a c b a)
(define (remove-first s los)
  (cond ((null? los) '())
        ((eqv? s (car los)) (cdr los))
        (else
         (cons (car los) (remove-first s (cdr los))))))

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
    ((identifier? lcexp)
     (eqv? sym lcexp))
    ((lambda? lcexp)
     (and (not (bound? sym lcexp))
          (occurs-free? sym (body lcexp))))
    ((application? lcexp)
     (or (occurs-free? sym (first lcexp))
         (occurs-free? sym (second lcexp))))
    (else
     (error 'occurs-free? "Invalid lambda calculus expression ~s" lcexp))))

;; 1.2.5 subst
;; (subst 'hi 'bye '(say hola hi bye ((say bye) bye) bye))
;; Slist ::= () | (Sexp . Slist)
;; Sexp  ::= Symbol | Slist
(define (subst new old slist)
  (cond ((null? slist) '())
        ((list? slist)
         (let [(sexp (car slist))]
           (cons
            (if (symbol? sexp)
                (if (eqv? old sexp) new sexp)
                (subst new old sexp))
            (subst new old (cdr slist)))))
        (else
         (error 'subst "Invalid s-list ~s" slist))))

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
  (let [(n (vector-length v))]
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
      (let [(a (first (car lst)))
            (b (second (car lst)))]
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
             (cond ((eqv? a x) b)
                   ((eqv? b x) a)
                   (else x)))) lst))

;; 1.19 listset: List x Int x Any -> List
;; Replaces the list item at the given index with the supplied value
;; Here's an opportunity to observe the 'no mysterious auxiliaries' rule,
;; but we will do a little 'software engineering' and use letrec so as
;; not to pollute the namespace. We also avoid visiting the elements
;; past the index (an obvious optimization).
(define (listset lst n val)
  (letrec [(listset-from
            (lambda (lst n val cur)
              (cond ((null? lst) '())
                    ((= cur n)
                     (cons val (cdr lst)))
                    (else
                     (cons (car lst)
                           (listset-from (cdr lst) n val (+ cur 1)))))))]
    (listset-from lst n val 0)))

;; 1.20 count-occurrences: Symbol x SList -> Int
;; Counts the occurrences of the given symbol anywhere in the SList
(define (count-occurrences s slist)
  (letrec [(co-from
            (lambda (s slist acc)
              (cond ((null? slist) acc)
                    ((symbol? (car slist))
                     (co-from s (cdr slist) (if (eqv? s (car slist))
                                                (+ acc 1)
                                                acc)))
                    (else
                     (+ (co-from s (car slist) acc)
                        (co-from s (cdr slist) acc))))))]
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
  (letrec [(index-of
             (lambda (pred lst cur)
               (cond ((null? lst) -1)
                     ((pred (car lst)) cur)
                     (else
                      (index-of pred (cdr lst) (+ cur 1))))))
           (is-found
            (lambda (n)
              (if (<= 0 n) n #f)))]
    (is-found (index-of pred lst 0))))
