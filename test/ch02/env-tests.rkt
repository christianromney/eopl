#lang racket
(require rackunit
         rackunit/text-ui
         (prefix-in h/ "../../src/ch02/envs/hash.rkt")
         (prefix-in c/ "../../src/ch02/envs/closure.rkt")
         (prefix-in d/ "../../src/ch02/envs/datatype.rkt"))

;; Define the test suite
(define env-test-suite
  (lambda (apply-fn extend-fn empty-fn)
    (let ([env-a (extend-fn
                  'x 10
                  (extend-fn
                   'y 1
                   (extend-fn
                    'x 1
                    (empty-fn))))])

      (test-suite
       "Environment lookup"
       (test-equal? "Lookup rebound var"
                    (apply-fn env-a 'x) 10)

       (test-equal? "Lookup nested bound var"
                    (apply-fn env-a 'y) 1)

       (test-exn "Invalid environment" exn?
                 (lambda ()
                   (apply-fn
                    (extend-fn 'a 1 "I-am-an-invalid-env") 'b)))

       (test-exn "No binding found" exn?
                 (lambda ()
                   (apply-fn env-a 'z)))))))


;; Macro for running the test suite against multiple implementations
(define-syntax run-for-all
  (syntax-rules ()
    [(_ suite ((a b c) ...))
     (begin
       (run-tests (suite a b c)) ...)]))

;; Run all the tests
(run-for-all env-test-suite
             ((h/apply-env h/extend-env h/empty-env)
              (c/apply-env c/extend-env c/empty-env)
              (d/apply-env d/extend-env d/empty-env)))
