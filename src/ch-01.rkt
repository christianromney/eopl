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
