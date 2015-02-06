#lang racket
;about (multirember a lat)
;a doesn't change when multirember recurs
;why not find a way to not need to state a as an argument every single time

(define multirember
  (lambda (a lat)
    ((letrec ((mr (lambda (lat)
                   (cond ((null? lat) '())
                         ((eq? a (car lat)) (mr (cdr lat)))
                         (else (cons (car lat) (mr (cdr lat))))))))
      mr)
    lat)))
;...why not an internal definition?...
;letrec has two parts, the naming part (mr is ...) and the value part
;unlike a defined function, function introduced in the naming part of letrec knows all the arguments of all the surrounding lambdas
;...but SICP uses internal definitions, and internally defined functions can use the arguments of the overarching function...

;rember, as defined earlier, works with lists of atoms only
;but we can have a more general version, which takes whatever test of equality as its argument, and returns a version of rember using this test
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond ((null? l) '())
            ((test? (car l) a) (cdr l))
            (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define multirember-f
  (lambda (test?)
    (lambda (a l)
      ((letrec ((mr (lambda (l)
                      (cond ((null? l) '())
                            ((test? a (car l)) (mr (cdr l)))
                            (else (cons (car l) (mr (cdr l))))))))
         mr)
       l))))

;but the function returned by (multirember-f test?) doesn't change when recurring on l
;we could use letrec, and name within it the function taking a and l


(define multirember-f2
  (lambda (test?)
    (letrec ((m-f (lambda (a l)
                     (cond ((null? l) '())
                           ((test? (car l) a) (m-f a (cdr l)))
                           (else (cons (car l) (m-f a (cdr l))))))))
     m-f)))
;...all's fine, but we're doing the whole letrec business to avoid passing arguments which do not change
;...and we're still passing a to m-f again and again

;(multirember-f2 eq?) returns the usual multirember, and more precisely
(define x
  (letrec ((m-f (lambda (a l)
                  (cond ((null? l) '())
                        ((eq? (car l) a) (m-f a (cdr l)))
                        (else (cons (car l) (m-f a (cdr l))))))))
    m-f))

;m-f is exactly like multirember

;now let's use letrec on member?
(define member?
  (lambda (a l)
    ((letrec ((m (lambda (l)
                  (cond ((null? l) #f)
                        ((eq? (car l) a) #t)
                        (else (m (cdr l)))))))
     m)
    l)))

(define union-1
  (lambda (set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1);the book doesn't use this clause. It's not neccesary, but why go through all the operations when you don't need to?
          ((member? (car set1) set2) (union-1 (cdr set1) set2))
          (else (cons (car set1) (union-1 (cdr set1) set2))))))
;union is written in such a way, that value of set2 doesn't change throughout evaluation
;we do things with elements of set1, and when set1 is finally empty, we take the whole set2 and cons stuff onto it

(define union-2
  (lambda (set1 set2)
    (letrec ((u (lambda (set)
                  (cond ((null? set) set2)
                        ((member? (car set) set2) (cons (car set) (u (cdr set))))
                        (else (u (cdr set)))))))
      (u set1))))

;union depends on member?, which is defined publicly
;what is sth about member? changes, let's say it takes arguments in another order?
;union would no longer work
;maybe we could avoid assuming stuff about member? when writing union
;we can define member? in the naming part of letrec
;that way it will never change unexpectedly, and union will be self-reliant
;SS calls it function protection 
;...yeah, that's good, but do we really want to define member? internally in every function which uses it (and there might be many)?...

(define union-3
  (lambda (set1 set2)
    (letrec ((member? (lambda (a l)
                        (cond ((null? l) #f)
                              ((eq? (car l) a) #t)
                              (else (member? a (cdr l))))))
             (u (lambda (set)
                  (cond ((null? set) set2)
                        ((member? (car set) set2) (cons (car set) (u (cdr set))))
                        (else (u (cdr set)))))))
      (u set1))))

;but the internal member? passes on an unchanging argument
;let's ameliorate that
(define union
  (lambda (set1 set2)
    (letrec ((member? (lambda (a lat)
                        (letrec ((m? (lambda (l)
                        (cond ((null? l) #f)
                              ((eq? (car l) a) #t)
                              (else (member? a (cdr l)))))))
                          (m? lat))))
             (u (lambda (set)
                  (cond ((null? set) set2)
                        ((member? (car set) set2) (cons (car set) (u (cdr set))))
                        (else (u (cdr set)))))))
      (u set1))))

;In chapter 1 we wrote two-in-a-row? which uses an auxiliary function two-in-a-row-b?
(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond ((null? lat) #f)
          (else (or (eq? (car lat) preceding) (two-in-a-row-b? (car lat) (cdr lat)))))))

(define two-in-a-row?-1
  (lambda (lat)
    (cond ((null? lat) #f)
          (else (two-in-a-row-b? (car lat) (cdr lat))))))
;two-in-a-row-b? should be an internal function

(define two-in-a-row?
  (lambda (lat)
    (letrec ((foo (lambda (preceding lat)
                  (cond ((null? lat) #f)
                        (else (or (eq? (car lat) preceding) (foo (car lat) (cdr lat))))))))
      (cond ((null? lat) #f)
            (else (foo (car lat) (cdr lat)))))))

;since foo doesn't need to know about two-in-a-row? arguments, we could have placed the letrec expression directly after (define two-in-a-row?

(define sum-of-prefixes
  (lambda (tup)
  (letrec ((foo (lambda (sonssf tup)
                  (cond ((null? tup) '())
                        (else (cons (+ (car tup) sonssf)
                                    (foo (+ (car tup) sonssf) (cdr tup))))))))
    (foo 0 tup))))


(define scramble
  (lambda (tup)
    (letrec ((pick (lambda (n lat)
                     (cond ((= n 1) (car lat))
                           (else (pick (- n 1) (cdr lat))))))
             (P (lambda (tup rp)
                  (cond ((null? tup) '())
                        (else (cons (pick (car tup) (cons (car tup) rp))
                                    (P (cdr tup) (cons (car tup) rp))))))))
      (P tup '()))))
        

