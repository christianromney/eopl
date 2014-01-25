#lang racket
(require rackunit
         rackunit/text-ui
         "../test-helper.rkt"
         (prefix-in l/ "../../src/ch02/stack/stack.rkt")
         (prefix-in d/ "../../src/ch02/stack/datatype.rkt"))

(define (stack-suite empty-stack empty-stack? push pop top)
  (let ([t (push 4 (push 3 (empty-stack)))])
    (test-suite
     "Tests for the stack interface"

     (test-true
      "Recognize the empty stack"
      (empty-stack? (empty-stack)))

     (test-exn
      "Can't read top of empty stack" exn?
      (lambda ()
        (top (empty-stack))))

     (test-exn
      "Can't pop empty stack" exn?
      (lambda ()
        (pop (empty-stack))))

     (test-true
      "Recognize arriving at empty stack"
      (empty-stack? (pop (pop t))))

     (test-equal?
      "Last value pushed should be the top"
      (top t) 4)

     (test-equal?
      "Popping should make previous value top"
      (let ([s (pop t)])
        (top s)) 3))))

(run-for-all stack-suite
             ((l/empty-stack l/empty-stack? l/push l/pop l/top)
              (d/empty-stack d/empty-stack? d/push d/pop d/top)))
