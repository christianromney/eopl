#lang racket
(require rackunit
         rackunit/text-ui
         (prefix-in a/ "../../src/ch02/envs/alist.rkt")
         (prefix-in b/ "../../src/ch02/envs/book.rkt")
         (prefix-in c/ "../../src/ch02/envs/closure.rkt")
         (prefix-in d/ "../../src/ch02/envs/datatype.rkt")
         (prefix-in h/ "../../src/ch02/envs/hash.rkt")
         (prefix-in r/ "../../src/ch02/envs/ribcage.rkt"))

;; Macro for running the test suite against multiple implementations
(define-syntax run-for-all
  (syntax-rules ()
    [(_ suite ((f ...) ...))
     (begin
       (run-tests (suite f ...)) ...)]))

;; Define the test suite
(define (base-test-suite apply-fn extend-fn empty-fn empty-pred)
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
                 (apply-fn env-a 'z))))))

;; Run all the tests
(run-for-all base-test-suite
             ((a/apply-env a/extend-env a/empty-env a/empty-env?)
              (b/apply-env b/extend-env b/empty-env b/empty-env?)
              (c/apply-env c/extend-env c/empty-env c/empty-env?)
              (d/apply-env d/extend-env d/empty-env d/empty-env?)
              (h/apply-env h/extend-env h/empty-env h/empty-env?)
              (r/apply-env r/extend-env r/empty-env r/empty-env?)))

(define (has-binding-tests has-fn extend-fn empty-fn)
  (let ([e (extend-fn
            'a 10
            (extend-fn
             'b 20
             (extend-fn
              'c 30
              (empty-fn))))])

    (test-suite
     "Validate has-binding? implementation"

     (test-true "Found existing binding"
                (has-fn e 'c))

     (test-false "Bogus binding not found"
                 (has-fn e 'z)))))

;; The closure-based implementation (c/XXX) does not
;; support has-binding?
(run-for-all has-binding-tests
             ((a/has-binding? a/extend-env a/empty-env)
              (b/has-binding? b/extend-env b/empty-env)
              (d/has-binding? d/extend-env d/empty-env)
              (h/has-binding? h/extend-env h/empty-env)
              (r/has-binding? r/extend-env r/empty-env)))

(define (extend-env*-tests apply-fn ext*-fn empty-fn)
  (let ([e (ext*-fn '(a b c) '(1 2 3) (empty-fn))])
    (test-suite "extend-env* should bind multiple vars"

                (test-equal? "should find last bound var"
                             (apply-fn e 'c) 3)

                (test-equal? "should find first bound var"
                             (apply-fn e 'a) 1)

                (test-exn "Can't find what's not there"
                          exn?
                          (lambda ()
                            (apply-fn e 'z))))))

;; Only the association list has extend-env* as of
;; Exercise 2.10
(run-for-all extend-env*-tests
             ((a/apply-env a/extend-env* a/empty-env)
              (r/apply-env r/extend-env* r/empty-env)))
