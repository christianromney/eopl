#lang racket
(require rackunit
         rackunit/text-ui
         "../../src/ch02/envs/datatype.rkt")

(define env-a
  (extend-env
   'x 10
   (extend-env
    'y 1
    (extend-env
     'x 1
     (empty-env)))))

(run-tests
 (test-suite
  "Environment lookup"
  (test-equal? "Lookup rebound var"
               (apply-env env-a 'x) 10)

  (test-equal? "Lookup nested bound var"
               (apply-env env-a 'y) 1)

  (test-exn "Invalid environment" exn?
            (lambda ()
              (apply-env
               (extend-env 'a 1 "I-am-an-invalid-env") 'b)))

  (test-exn "No binding found" exn?
            (lambda ()
              (apply-env env-a 'z)))))
