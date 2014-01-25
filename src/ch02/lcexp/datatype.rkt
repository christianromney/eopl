#lang eopl

;; Straight from p.46 of book

;; constructors
(define-datatype lc-exp lc-exp?
  (var-exp (var identifier?))
  (lambda-exp (bound-var identifier?)
              (body lc-exp?))
  (app-exp (rator lc-exp?)
           (rand lc-exp?)))

;; occurs-free?
(define (occurs-free? search-var exp)
  (cases lc-exp exp
         (var-exp (var) (eq? var search-var))
         (lambda-exp (bound-var body)
                     (and (not (eq? search-var bound-var))
                          (occurs-free? search-var body)))
         (app-exp (rator rand)
                  (or (occurs-free? search-var rator)
                      (occurs-free? search-var rand)))))

(provide lc-exp? var-exp lambda-exp app-exp occurs-free?)
