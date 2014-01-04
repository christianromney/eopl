#lang racket
(require rackunit
         rackunit/text-ui
         (prefix-in a/ "../../src/ch02/envs/alist.rkt")
         (prefix-in b/ "../../src/ch02/envs/book.rkt")
         (prefix-in c/ "../../src/ch02/envs/closure.rkt")
         (prefix-in d/ "../../src/ch02/envs/datatype.rkt")
         (prefix-in h/ "../../src/ch02/envs/hash.rkt"))

;; Define the test suite
(define env-test-suite
  (lambda (apply-fn extend-fn empty-fn empty-pred)
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

       (test-true "Empty is empty"
                  (empty-pred (empty-fn)))

       (test-false "Extended env is not empty"
                   (empty-pred env-a))

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
    [(_ suite ((a b c d) ...))
     (begin
       (run-tests (suite a b c d)) ...)]))

;; Run all the tests
(run-for-all env-test-suite
             ((a/apply-env a/extend-env a/empty-env a/empty-env?)
              (b/apply-env b/extend-env b/empty-env b/empty-env?)
              (c/apply-env c/extend-env c/empty-env c/empty-env?)
              (d/apply-env d/extend-env d/empty-env d/empty-env?)
              (h/apply-env h/extend-env h/empty-env h/empty-env?)))
