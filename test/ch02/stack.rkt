#lang racket
(require rackunit
         rackunit/text-ui
         "../../src/ch02/stack/stack.rkt")

(define stack-suite
  (let ([t (push 4 (push 3 (empty-stack)))])
    (test-suite
     "Tests for the stack interface"

     (test-true
      "Recognize the empty stack"
      (empty-stack? (empty-stack)))

     (test-equal?
      "Last value pushed should be the top"
      (top t) 4)

     (test-equal?
      "Popping should make previous value top"
      (let ([s (pop t)])
        (top s)) 3))))

(run-tests stack-suite)
