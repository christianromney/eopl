#lang eopl

;; ============
;; LET Language
;; ============
;;
;; The lexical rules defines the characters allowed to appear in the source
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
    (expression ("zero?" "(" expression ")") zero?-expression)
    (expression ("if" expresssion "then" expression "else" expression) if-expression)
    (expression (identifier) variable-expression)
    (expression ("let" identifier "=" expression "in" expression) let-expression)))

(define scan&parse
  (sllgen:make-string-parser lexical-rules grammar))

;; -- Abstract Data Types --

;; -- expression values --

(define-datatype expression-value expression-value?
  (number-value (num number?))
  (boolean-value (bool boolean?)))

(define expression-value->number
  (lambda (ev)
    (cases expression-value ev
           (number-value (num) num)
           (else (eopl:error "expression-value->integer only works with integer-values")))))

(define expression-value->boolean
  (lambda (ev)
    (cases expression-value ev
           (boolean-value (bool) bool)
           (else (eopl:error "expression-value->boolean only works with boolean-values")))))

;; -- expressions --

(define-datatype expression expression?
  (constant-expression [num number?])
  (diff-expression [exp1 expression?]
                   [exp2 expression?])
  (zero?-expression [exp expression?])
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

           (diff-expression (exp1 exp2)
                            (let ([val1 (value-of exp1 env)]
                                  [val2 (value-of exp2 env)])
                              (number-value
                               (- (expression-value->number val1)
                                  (expression-value->number val2)))))

           (zero?-expression (exp)
                             (if (zero? (expression-value->number (value-of exp env)))
                                 (boolean-value #t)
                                 (boolean-value #f)))

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

;; Unwraps the final expressed value, returning a number or boolean
(define eval-program
  (lambda (pgm)
    (let ([val (value-of-program pgm)])
      (cases expression-value val
             (number-value (num) num)
             (boolean-value (bool) bool)))))

;; A REPL for our language!
(define repl
  (sllgen:make-rep-loop
   "LET-LANG> "
   (lambda (pgm) (eval-program pgm))
   (sllgen:make-stream-parser lexical-rules grammar)))
