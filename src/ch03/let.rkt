#lang eopl

;; ============
;; LET Language
;; ============
;;
;; The lexical rules define the characters allowed to appear in the source
;; code of a program. The lexical rules are used by SLLGEN to generate a scanner.
;; In the following grammar, Capitalized text represents a non-terminal, and lowercase
;; text and punctuation represent terminals.
;;
;; LexicalRules  ::= ( {Regexp+Action}* )
;; Regexp+Action ::= ( Name ( {Regexp}* ) Action )
;; Name          ::= Symbol
;; Regexp        ::= String>
;;                 | letter
;;                 | digit
;;                 | whitespace
;;                 | any
;; Regexp        ::= ( not Character )
;;                 | ( or {Regexp}* )
;; Regexp        ::= ( arbno Regexp )
;;                 | ( concat {Regexp}* )
;; Action        ::= skip | symbol | number | string

(define lexical-rules
  '((whitespace (whitespace) skip)
    (comment ("#" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    (number (digit (arbno digit)) number)))

;; Grammar The grammar is used to recognize groups of scanned tokens as parts of
;; syntax (expressions of a type). In the double-quoted text represents
;; non-terminals or tokens. The Lhs should be the name of an abstract data type,
;; the rhs items should be a combination of terminals and non-terminals. The terminals
;; should also be abstract data types. Finally, the production names correspond to
;; concreate variants of the abstract data type. In the case of the grammar below,
;; the ADTs <expression> and <program> are defined later in this file.
;;
;; Grammar ::= ( {Production}* )
;; Production ::= ( Lhs ( {Ritem}* ) ProductionName )
;; Lhs ::= Symbol
;; Ritem ::= Symbol
;;         | String
;; Ritem ::= ( arbno {Ritem}* )
;; Ritem ::= (separated-list {Ritem}* String)
;; ProductionName ::= Symbol

(define grammar
  '((program (expression) a-program)
    (expression (number) constant-expression)
    (expression ("-" "(" expression "," expression ")") diff-expression)
    (expression ("+" "(" expression "," expression ")") add-expression)
    (expression ("*" "(" expression "," expression ")") mult-expression)
    (expression ("/" "(" expression "," expression ")") quot-expression)
    (expression ("minus" "(" expression ")") minus-expression)
    (expression ("zero?" "(" expression ")") zero?-expression)
    (expression ("equal?" "(" expression "," expression ")") equal?-expression)
    (expression ("greater?" "(" expression "," expression ")") greater?-expression)
    (expression ("less?" "(" expression "," expression ")") less?-expression)
    (expression ("emptylist") null-expression)
    (expression ("null?" "(" expression ")") null?-expression)
    (expression ("cons" "(" expression "," expression ")") cons-expression)
    (expression ("car" "(" expression ")") car-expression)
    (expression ("cdr" "(" expression ")") cdr-expression)
    (expression ("if" expresssion "then" expression "else" expression) if-expression)
    (expression (identifier) variable-expression)
    (expression ("let" identifier "=" expression "in" expression) let-expression)))

(define scan&parse
  (sllgen:make-string-parser lexical-rules grammar))

;; -- Abstract Data Types --

;; -- expression values --

(define any?
  (lambda (x)
    #t))

(define-datatype expression-value expression-value?
  (number-value (num number?))
  (boolean-value (bool boolean?))
  (null-value)
  (pair-value (head any?)
              (tail any?)))

(define expression-value->number
  (lambda (ev)
    (cases expression-value ev
           (number-value (num) num)
           (else (eopl:error "expression-value->number only works with a number-value")))))

(define expression-value->boolean
  (lambda (ev)
    (cases expression-value ev
           (boolean-value (bool) bool)
           (else (eopl:error "expression-value->boolean only works with a boolean-value")))))

(define expression-value->null
  (lambda (ev)
    (cases expression-value ev
           (null-value () '())
           (else (eopl:error "expression-value->null only works with a null-value")))))

(define expression-value->pair
  (lambda (ev)
    (cases expression-value ev
           (pair-value (head tail) (cons head tail))
           (else (eopl:error "expression-value->pair only works with a pair-value")))))

;; -- expressions --

(define-datatype expression expression?
  (constant-expression [num number?])
  (minus-expression [exp expression?])
  (diff-expression [exp1 expression?]
                   [exp2 expression?])
  (add-expression [exp1 expression?]
                  [exp2 expression?])
  (mult-expression [exp1 expression?]
                   [exp2 expression?])
  (quot-expression [exp1 expression?]
                   [exp2 expression?])
  (zero?-expression [exp expression?])
  (equal?-expression [exp1 expression?]
                     [exp2 expression?])
  (greater?-expression [exp1 expression?]
                       [exp2 expression?])
  (less?-expression [exp1 expression?]
                    [exp2 expression?])
  (null-expression)
  (null?-expression [exp expression?])
  (cons-expression [exp1 expression?]
                   [exp2 expression?])
  (car-expression [exp expression?])
  (cdr-expression [exp expression?])
  (if-expression [pred expression?]
                 [consequent expression?]
                 [alternative expression?])
  (variable-expression [var symbol?])
  (let-expression [var symbol?]
                  [binding expression?]
                  [body expression?]))

;; -- Program --

;; Program ::= Expression

(define-datatype program program?
  (a-program [exp expression?]))

;; -- Environment --

(define-datatype environment environment?
  (empty-environment)
  (extended-environment
   [env environment?]
   [var symbol?]
   [val expression-value?]))

(define apply-environment
  (lambda (env var)
    (cases environment env
           (empty-environment
            ()
            (eopl:error "No binding for var in environment"))

           (extended-environment
            (prior-env bound-var bound-val)
            (if (eq? bound-var var)
                bound-val
                (apply-environment prior-env var))))))

;; -- Interpreter --

(define value-of
  (lambda (exp env)
    (cases expression exp
           (constant-expression (num)
                                (number-value num))

           (variable-expression (var)
                                (apply-environment env var))

           (minus-expression (num)
                             (let ([val (value-of num env)])
                               (number-value
                                (- 0 (expression-value->number val)))))

           (diff-expression (exp1 exp2)
                            (let ([val1 (value-of exp1 env)]
                                  [val2 (value-of exp2 env)])
                              (number-value
                               (- (expression-value->number val1)
                                  (expression-value->number val2)))))

           (add-expression (exp1 exp2)
                           (let ([val1 (value-of exp1 env)]
                                 [val2 (value-of exp2 env)])
                             (number-value
                              (+ (expression-value->number val1)
                                 (expression-value->number val2)))))

           (mult-expression (exp1 exp2)
                            (let ([val1 (value-of exp1 env)]
                                  [val2 (value-of exp2 env)])
                              (number-value
                               (* (expression-value->number val1)
                                  (expression-value->number val2)))))

           (quot-expression (exp1 exp2)
                            (let ([val1 (value-of exp1 env)]
                                  [val2 (value-of exp2 env)])
                              (number-value
                               (quotient (expression-value->number val1)
                                         (expression-value->number val2)))))

           (zero?-expression (exp)
                             (if (zero? (expression-value->number (value-of exp env)))
                                 (boolean-value #t)
                                 (boolean-value #f)))

           (equal?-expression (exp1 exp2)
                              (let ([val1 (value-of exp1 env)]
                                    [val2 (value-of exp2 env)])
                                (boolean-value
                                 (= (expression-value->number val1)
                                    (expression-value->number val2)))))

           (greater?-expression (exp1 exp2)
                                (let ([val1 (value-of exp1 env)]
                                      [val2 (value-of exp2 env)])
                                  (boolean-value
                                   (> (expression-value->number val1)
                                      (expression-value->number val2)))))

           (less?-expression (exp1 exp2)
                             (let ([val1 (value-of exp1 env)]
                                   [val2 (value-of exp2 env)])
                               (boolean-value
                                (< (expression-value->number val1)
                                   (expression-value->number val2)))))

           (null-expression () (null-value))

           (null?-expression (exp)
                             (let ([val (value-of exp env)])
                               (boolean-value
                                (cases expression-value val
                                       (null-value () #t)
                                       (else #f)))))

           (cons-expression (exp1 exp2)
                            (pair-value
                             (value-of exp1 env)
                             (value-of exp2 env)))

           (car-expression (exp)
                           (car (expression-value->pair (value-of exp env))))

           (cdr-expression (exp)
                           (cdr (expression-value->pair (value-of exp env))))

           (if-expression (pred conseq altern)
                          (if (expression-value->boolean (value-of pred env))
                              (value-of conseq env)
                              (value-of altern env)))

           (let-expression (var val body)
                           (value-of body
                                     (extended-environment env var (value-of val env)))))))

;; Returns an expressed value
(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp)
                      (value-of exp (empty-environment))))))

(define unwrap
  (lambda (val)
    (cases expression-value val
             (number-value (num) num)
             (boolean-value (bool) bool)
             (pair-value (head tail) (cons (unwrap head) (unwrap tail)))
             (null-value () '()))))

;; Unwraps the final expressed value, returning a number or boolean
(define eval-program
  (lambda (pgm)
    (let ([val (value-of-program pgm)])
      (unwrap val))))

;; A REPL for our language!
(define repl
  (sllgen:make-rep-loop
   "LET-LANG> "
   (lambda (pgm) (eval-program pgm))
   (sllgen:make-stream-parser lexical-rules grammar)))
