#lang racket

(require rackunit
         rackunit/text-ui)

;; Macro for running the test suite against multiple implementations

(define-syntax run-for-all
  (syntax-rules ()
    [(_ suite ((f ...) ...))
     (begin
       (run-tests (suite f ...)) ...)]))

(provide run-for-all)
