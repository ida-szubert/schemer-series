#lang racket

;TABLES
;a table could be represented as a function
;a table pairs names with values

(define the-empty-table
  (lambda (name)
    ...))

;if a table is a function, we apply the table to a name to get whatever is associated with that name
(define lookup
  (lambda (table name)
    (table name)))

(define extend
  (lambda (name1 value table)
    (lambda (name2)
      (cond ((eq? name2 name1) value)
            (else (table name2))))))
;extend takes a name, a value, and a table, and returns a function, which
;when given the same name again returns the input value
;and when given a different name, looks for the value in the table
;the function which is a result of extend is an extended table



;we don't know what a value of a definition is
;we might define it, the way we defined values of varius expressions in Chapter 10 of The Little Schemer
;we might also want a predicate define?, to check if we're dealing with a definition
(define define?
  (lambda (e)
    (cond ((atom? e) #f)
          ((atom? (car e)) (eq? (car e) 'define))
          (else #f))))

(define global-table
  ... the-empty-table...)

(define *define
  (lambda (e)
    (set! global-table (extend (name-of e)
                               (box (the-meaning (right-side-of e)))
                               global-table))))

;*define uses global table to remember those functions that were defined
;for some reason it puts the body of a definition in something called a box
(define box
  (lambda (it)
    (lambda (sel)
      (sel it (lambda (new)
                (set! it new))))))

;box takes the body of a definition as its argument
;and returns a function
;and in some complex way this function allows for the original body to be changed, i.e. putting the body in a box somehow facilitates redefining

(define setbox
  (lambda (box new)
    (box (lambda (it set) (set new)))))

;((lambda (sel)
;  (sel <body> (lambda (new) (set! <body> new))))
; (lambda (it set) (set <new-body>)))

;((lambda (it set) (set <new-body>)) <body> (lambda (new) (set! <body> new)))

;(set! <body> <new-body>)

;... That's seems to be a covoluted way of saying (set! <body> <new-body>)...
;... more interestingly, how do you come up with such functions?...

(define unbox
  (lambda (box)
    (box (lambda (it set) it))))

;so boxes are there so that the contents of the box can be changed
;the box itself doesn't change
;which means that the table doesn't change
;if bodies of definitions were not boxed, changing them would mean changing the table
;...for some reason it's better not to change the table...
;...maybe the point it that sth once defined stays the sam way forever...
;... in which case why is it smart to bypass this restriction?...

(define the-meaning
  (lambda (e)
    (meaning e lookup-in-global-table)))

;lookup-in-global-table is a function which takes the name e and finds the associated expression in the global table
;which is pretty much what we said a table does
(define lookup-in-global-table
  (lambda (name)
    (lookup global-table name)))

;but it's not exactly like global-table
;*define uses the-meaning when setting global-table to a new global-table
;"lookup-in-global-table is always just like the most recent global-table, not like the one we have now"

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

;expression-to-action takes e and returns a function which extracts meaning of e given e and a table
;expression-to-action "is easy and can wait untill later"

;here's one action
(define *quote
  (lambda (e table)
    (text-of e)))

(define *identifier
  (lambda (e table)
    (unbox (lookup table e))))

(define *set
  (lambda (e table)
    (setbox (lookup table e)
            (meaning (right-side-of e) table))))

(define *lambda
  (lambda (e table)
    (lambda (args)
      (beglis (body-of e)
              (multi-extend (formals-of e)
                            (box-all args)
                            table)))))

(define beglis
  (lambda (es table)
    (cond ((null? (cdr es)) (meaning (car es) table))
          (else ((lambda (val)
                  (beglis (cdr es) table))
                  (meaning (car es) table))))))

;if the body of e consists of one element only, beglis returns the meaning of that element
;(lambda (val)...) is like (let ((val (meaning (car es) table))))
;so that first we determine (meaning (car es) table), before calling beglis eith (cdr es)
;but val is never used, why do we bother computing it?
;"because the values of all but the last expression in the value part of a (lambda ...) are ignored
;... but we would determine the value of the last expression anyway, the ((null? (cdr es)) condition makes sure of that...
;... wouldn't it be easier to find the last expression and evaluate it, without bothering to do anything with other expressions?...

(define box-all
  (lambda (args)
    (cond ((null? args) '())
          (else (cons (box (car args))
                      (box-all (cdr args)))))))

;ok, here's what *lambda does: when given (lambda (x y z) ...)
;it returns a function, which takes the values of the arguments, extends table by pairing each formal name of the original (lambda ...)
;i.e x, y, and z
;with the given argument values
;and then, using beglist, determines the values of the expressions in the body of lambda, and returns the value of the last expression

(define multi-extend
  (lambda (names values table)
    (cond ((null? names) table)
          (else (let ((new-table (extend (car names) (car values) table)))
                  (multi-extend (cdr names) (cdr values) new-table))))))
;mine is an iterative process, SS use a recursive one:
(define multi-extendSS
  (lambda (names values table)
    (cond ((null? names) table)
          (else (extend (car names)
                        (car values)
                        (multi-extend (cdr names) (cdr values) table))))))


(define *application
  (lambda (e table)
    ((meaning (function-of e) table)
     (evlis (arguments-of e) table))))
;that's straightforward
;apply the value of the forst expression to the values of the rest of the expressions
;evlis evaluates a list of expressions and returns a list of values

(define evlis-my
  (lambda (l table)
    (cond ((null? l) '())
          (else (cons (meaning (car l) table)
                      (evlist (cdr l) table))))))

(define evlis
  (lambda (l table)
    (cond ((null? l) '())
          (else (lambda (val)
                  (cons val
                        (evlist (cdr l) table)))
                (meaning (car l) table)))))

;let's consider (value (car (cons 0 '())))
;value uses the-meaning, which uses meaning, which uses expression-to-action
;the expression is an application, so *application is used
;(meaning ('car table)) must be a function
(define :car
  (lambda (args-in-a-list)
    (car (car args-in-a-list))))

(define a-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list)))))

(define b-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list) (cadr args-in-a-list)))))

(define *const-1
  (lambda (e table)
    (cond ((number? e) e)
          ((eq? e #t) #t)
          ((eq? e #f) #f)
          ((eq? e 'cons) (b-prim cons))
          ((eq? e 'car) (a-prim car))
          ((eq? e 'cdr) (a-prim cdr))
          ((eq? e 'eq?) (b-prim eq?))
          ((eq? e 'atom?) (a-prim atom?))
          ((eq? e 'null?) (a-prim null?))
          ((eq? e 'zero?) (a-prim zero?))
          ((eq? e 'add1) (a-prim add1))
          ((eq? e 'sub1) (a-prim sub1))
          ((eq? e 'number?) (a-prim number?)))))

;it's not ideal, because every time *const is used, computation involving a/b-prim needs to be performed, even though the result is always the same
(define *const
    (let ((:cons (b-prim cons))
          (:car (a-prim car))
          (:cdr (a-prim cdr))
          (:eq? (b-prim eq?))
          (:atom? (a-prim atom?))
          (:null? (a-prim null?))
          (:zero? (a-prim zero?))
          (:add1 (a-prim add1))
          (:sub1 (a-prim sub1))
          (:number? (a-prim number?)))
      (lambda (e table)
        (cond ((number? e) e)
          ((eq? e #t) #t)
          ((eq? e #f) #f)
          ((eq? e 'cons) :cons)
          ((eq? e 'car) :car)
          ((eq? e 'cdr) :cdr)
          ((eq? e 'eq?) :eq?)
          ((eq? e 'atom?) :atom?)
          ((eq? e 'null?) :null?)
          ((eq? e 'zero?) :zero?)
          ((eq? e 'add1) :add1)
          ((eq? e 'sub1) :sub1)
          ((eq? e 'number?) :number?)))))

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define evcon
  (lambda (lines table)
    (cond ((eq? 'else (condition-of (car lines)))
           (meaning (answer-of (car lines))))
          ((meaning (question-of (car lines)) table)
           (meaning (answer-of (car lines))))
          (else (evcon (cdr lines) table)))))

(define *letcc
  (lambda (e table)
    (let/cc skip
      (beglis (ccdoby-of e)
              (extend (name-of e)
                      (box (a-prim skip))
                      table)))))

;what happens when we ask for a value using a name which has not been defined?
;when sth wrong like that happens, we might want to forget all pending computations
;maybe by using (let/cc...)?

(define value
  (lambda (e)
    (let/cc the-end
      (set! abort the-end)
      (cond ((define? e) (*define e))
            (else (the-meaning e))))))

(define abort '())

(define the-empty-table
  (lambda (name)
    (abort (cons 'no-answer (cons name '())))))
    
;if the table is empty, value returns no-answer (e)

(define expression-to-action
  (lambda (e)
    (cond ((atom? e) (atom-to-action e))
          (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond ((number? e) *const)
          ((eq? e #t) *const)
          ((eq? e #f) *const)
          ((eq? e 'cons) *const)
          ((eq? e 'car) *const)
          ((eq? e 'cdr) *const)
          ((eq? e 'null?) *const)
          ((eq? e 'eq?) *const)
          ((eq? e 'atom?) *const)
          ((eq? e 'zero?) *const)
          ((eq? e 'add1) *const)
          ((eq? e 'sub1) *const)
          ((eq? e 'number?) *const)
          (else *indentifier))))

(define list-to-action
  (lambda (e)
    (cond ((atom? (car e)) (cond ((eq? (car e) 'quote) *quote)
                                 ((eq? (car e) 'lambda) *lambda)
                                 ((eq? (car e) 'let/cc) *letcc)
                                 ((eq? (car e) 'set!) *set)
                                 ((eq? (car e) 'cond) *cond)
                                 (else *application)))
          (else *application))))

(define text-of
  (lambda (x)
    (cadr x)))

(define formals-of
  (lambda (x)
    (cadr x)))

(define body-of
  (lambda (x)
    (cddr x)))

(define ccbody-of
  (lambda (x)
    (cddr x)))

(define name-of
  (lambda (x)
    (cadr x)))

(define right-side-of
  (lambda (x)
    (cond ((null? (cddr x)) 0);this assumes that definitions without right-hand side like (define abort) are allowed. A value is made up - 0
          (else (cddr x)))))

(define cond-lines-of
  (lambda (x)
    (cdr x)))

(define else?
  (lambda (x)
    (and (atom? x) (eq? x 'else))))

(define question-of
  (lambda (x)
    (car x)))

(define answer-of
  (lambda (x)
    (cadr x)))

(define function-of
  (lambda (x)
    (car x)))

(define arguments-of
  (lambda (x)
    (cdr x)))

;Can we teach value what value means?
;so that (value (value 1)) doesn't return (no-answer value)
;it will happen if we add the neccessary functions to the global-table, including define?, *define, the-meaning, lookup, lookup-in-global-table
;(value 1) is not a definition, so we need to determine (the-meaning (value 1))
;(value 1) is an application, so we need to determine (meaning value table); since (define? 1) is #f, we need (the-meaning 1)
;which eventually evaluates to 1
;thus (value 1) is 1, and (value (value 1)) is 1 as well
         
                      


     
  
