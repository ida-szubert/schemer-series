#lang racket
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name (first entry) (second entry) entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond ((null? names) (entry-f name))
          ((eq? name (car names)) (car values))
          (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond ((null? table) (table-f name))
          (else (lookup-in-entry name (car table) (lambda (name)
                                                    (lookup-in-entry name (cdr table) table-f)))))))

;*const - numerical, truth values, primitive functions, 
;*quote - quote operation
;*identifier - names of variables?
;*lambda -lambda expressions
;*application - (function arguments)
;*cond - cond expressions

; these types can be representet by functions, which describe what is a natural value of expressions of a given type

(define expression-to-action
  (lambda (e)
    (cond ((atom? e) (atom-to-action e))
          (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond ((number? e) *const)
          ((eq? #f e) *const)
          ((eq? #t e) *const)
          ((eq? (quote car) e) *const)
          ((eq? (quote cdr) e) *const)
          ((eq? (quote eq?) e) *const)
          ((eq? (quote zero?) e) *const)
          ((eq? (quote null?) e) *const)
          ((eq? (quote atom?) e) *const)
          ((eq? (quote cons) e) *const)
          ((eq? (quote number?) e) *const)
          ((eq? (quote add1) e) *const)
          ((eq? (quote add2) e) *const)
          (else *identifier))))

(define list-to-action
  (lambda (l)
    (cond ((atom? (car l)) (cond ((eq? (car l) (quote quote)) *quote)
                                 ((eq? (car l) (quote lambda)) *lambda)
                                 ((eq? (car l) (quote cond)) *cond))
                           (else *application))
          (else *application))))

(define value
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e  table)
    ((expression-to-action e) e table)))
;this works because (expression-to-action e) outputs a function, e.g. *const

(define *const
  (lambda (e table)
    (cond ((number? e) e)
          ((eq? e #t) #t)
          ((eq? e #f) #f)
          (else (build (quote primitive) e)))))
;the value of primitive functions is e.g. "primitive car"

(define *quote
  (lambda (e table)
    (text-of e)))
(define text-of
  (lambda (e)
    (second e)))
;because e is an expression of the shape (quote (you should better weed the garden)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))
(define initial-table
  (lambda (name)
    (car '())))
;when evaluing an identifier, we want to check in the table what is the value associated with that name
;initial-table is used when the identifier has not been found among the names in the table

(define *lambda
  (lambda (e table)
    (build (quote non-primitive) (cons table (cdr e)))))
;so the value of a function looks like this:
;(non-primitive (((y z) ((8) 9))) (x) (cons x y))), i.e. table followed by formals, followed by body

(define table-of first)
(define formals-of second)
(define body-of third)

(define evcon
  (lambda (lines table)
    (cond ((else? (question-of (car lines))) (meaning (answer-of (car lines)) table))
          ((meaning (question-of (car lines)) table) (meaning (answer-of (car lines)) table))
          (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond ((atom? x) (eq? x (quote else)))
          (else #f))))
(define question-of first)
(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))
(define cond-lines-of cdr)
; to find the value of the conditionals, we need to have a table in which values of identifiers are stored

(define evlis
  (lambda (args table)
    (cond ((null? args) '())
          (else (cons (meaning (car args) table) (evlis (cdr ards) table))))))

(define *application
  (lambda (e table)
    (apply (mening (function-of e) table) (evlis (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (car l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (car l) (quote non-primitive))))

(define apply
  (lambda (fun vals)
    (cond ((primitive? fun) (apply-primitive (second fun) vals))
          ((non-primitive?fun) (apply-closure (second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond ((eq? name (quote cons)) (cons (first vals) (second vals)))
          ((eq? name (quote car)) (car (first vals)))
          ((eq? name (quote cdr)) (cdr (first vals)))
          ((eq? name (quote null?)) (null? (first vals)))
          ((eq? name (quote eq?)) (eq? (first vals) (second vals)))
          ((eq? name (quote zero?)) (zero? (first vals)))
          ((eq? name (quote number?)) (number? (first vals)))
          ((eq? name (quote add1)) (add1 (first vals)))
          ((eq? name (quote sub1)) (sub1 (first vals)))
          ((eq? name (quote atom?)) (:atom? (first vals))))))

(define :atom?
  (lambda (x)
    (cond ((atom? x) #t)
          ((null? x) #f)
          ((eq? (car x) (quote primitive)) #t)
          ((eq? (car x) (quote non-primitive)) #t)
          (else #f))))
; is function an atom?

; we know how to find the meaning of an application given its body and and its table of varaibles and values
; given a non-primitive function we find the meaning of its body, with the table extended by the entry ((formals) (values))
; meaning needs an expression and a table
(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure) (extended-table (new-entry (formals-of closure) vals) (table-of closure)))))


          
          
    
                                 