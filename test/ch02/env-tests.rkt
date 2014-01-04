#lang racket
(require rackunit
         rackunit/text-ui
         "../../src/ch02/envs/closure.rkt")

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

  (test-exn "No binding" exn?
            (lambda ()
              (apply-env env-a 'z)))))
