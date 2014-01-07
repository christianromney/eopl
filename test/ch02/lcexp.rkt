#lang racket
(require rackunit
         rackunit/text-ui
         "../../src/ch02/lcexp/shorthand.rkt")

(define lambda-calc-suite
  (let ([lam (lambda-exp 'x (app-exp (var-exp 'even?)
                                     (var-exp 2)))])
    (test-suite
     "Tests for the lambda calculus interface"

     (test-true "Should recognize var-exp"
                (var-exp? (var-exp 'a)))

     (test-true "Should recognize lambda-exp"
                (lambda-exp? (lambda-exp 'a (var-exp 'a))))

     (test-true "Should recognize app-exp"
                (app-exp?
                 (app-exp
                  (lambda-exp 'a (var-exp 'a))
                  (var-exp 1))))

     (test-equal? "Should extract bound var"
                  (lambda-exp->bound-var lam) 'x)

     (test-equal? "Should extract function from application"
                  (var-exp->var (app-exp->rator (lambda-exp->body lam)))
                  'even?)

     (test-equal? "Should extract argument from application"
                  (var-exp->var (app-exp->rand (lambda-exp->body lam)))
                  2)

     (test-true "Should recognize free variable as identifier"
                (occurs-free? 'a (var-exp 'a)))

     (test-true "Should recognize free variables in lambda expression"
                (occurs-free? 'a
                              (lambda-exp 'x
                                          (app-exp (var-exp 'display)
                                                   (var-exp 'a)))))

     (test-false "Should recognize bound variables are not free"
                 (occurs-free? 'a
                               (lambda-exp 'a
                                           (app-exp (var-exp 'display)
                                                    (var-exp 'a)))))

     (test-true "Should recognize free variables in application expression"
                (occurs-free? 'display
                              (lambda-exp 'x
                                          (app-exp (var-exp 'display)
                                                   (var-exp 'a)))))

     )))

(run-tests lambda-calc-suite)
