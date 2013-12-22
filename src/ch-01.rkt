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
  (letrec [(subst-in-sexp
            (lambda (sexp)
              (if (symbol? sexp)
                  (if (eqv? old sexp) new sexp)
                  (subst-map new old sexp))
              ))]
    (map subst-in-sexp slist)))
