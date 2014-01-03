#lang racket
(require rackunit
         "../../src/ch02/envs/closure.rkt")

(define env-a
   (extend-env 'x 10
             (extend-env 'y 1
                         (extend-env 'x 1
                                     (empty-env)))))

(check-equal? (apply-env env-a 'x) 10)
