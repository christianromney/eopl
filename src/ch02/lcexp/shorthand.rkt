#lang eopl
;; Lambda Calculus Representation
;;
;; Constructors
;;
;; var-exp: Var -> LcExp
(define (var-exp var)
  var)

;; lambda-exp: Var x LcExp -> LcExp
(define (lambda-exp var lcexp)
  (list 'λ var "." lcexp))

;; app-exp: LcExp x LcExp -> LcExp
(define (app-exp rator rand)
  (list rator rand))

;; Predicates
;;
;; var-exp?: LcExp -> Bool
(define (var-exp? lcexp)
  (and (symbol? lcexp)
       (not (eqv? lcexp 'λ))))

;; lambda-exp?: LcExp -> Bool
(define (lambda-exp? lcexp)
  (and (list? lcexp)
       (= 4 (length lcexp))
       (eqv? (car lcexp) 'λ)))

;; app-exp?: LcExp -> Bool
(define (app-exp? lcexp)
  (and (list? lcexp)
       (not (lambda-exp? lcexp))
       (= 2 (length lcexp))))

;; Extractors
;;
;; var-exp->var: LcExp -> Var
(define (var-exp->var lcexp)
  lcexp)

;; lambda-exp->bound-var: LcExp -> Var
(define (lambda-exp->bound-var lcexp)
  (cadr lcexp))

;; lambda-exp->body: LcExp -> LcExp
(define (lambda-exp->body lcexp)
  (cadddr lcexp))

;; app-exp->rator: LcExp -> LcExp
(define (app-exp->rator lcexp)
  (car lcexp))

;; app-exp->rand: LcExp -> LcExp
(define (app-exp->rand lcexp)
  (cadr lcexp))

;; From book:
;; occurs-free? : Sym × LcExp → Bool
(define occurs-free?
  (lambda (search-var exp)
    (cond
     [(var-exp? exp)
      (eqv? search-var (var-exp->var exp))]
     [(lambda-exp? exp)
      (and
       (not (eqv? search-var (lambda-exp->bound-var exp)))
       (occurs-free? search-var (lambda-exp->body exp)))]
     [else
      (or
       (occurs-free? search-var (app-exp->rator exp))
       (occurs-free? search-var (app-exp->rand exp)))])))

(provide var-exp? var-exp lambda-exp? lambda-exp app-exp? app-exp
         var-exp->var app-exp->rator app-exp->rand
         lambda-exp->body lambda-exp->bound-var occurs-free?)
