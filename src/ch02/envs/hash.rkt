#lang racket

;; This representation leverages Racket's hash tables
;; to store bindings. The one drawback to this representation
;; is that old bindings are not preserved, but that is not (yet)
;; listed as a requirement!
(define (empty-env)
  (hash))

(define (empty-env? env)
  (and (hash? env)
       (zero? (hash-count env))))

(define (extend-env var val saved)
  (hash-set saved var val))

(define (apply-env env search)
  (let ([ans (hash-ref env search 'apply-env/not-found)])
    (cond [(eqv? ans 'apply-env/not-found)
           (error 'apply-env "No binding found for ~s" search)]
          [(not (hash? env))
           (error 'apply-env "Invalid environment ~s" env)]
          [else ans])))

(provide empty-env extend-env apply-env empty-env?)
