#lang racket

(define member? 
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (or (eq? a (car lat)) (member? a (cdr lat)))))))

;(define two-in-a-row
;  (lambda (lat)
;    (cond ((null? lat) #f)
;          (else (or (eq? (car lat) (cadr lat)) (two-in-a-row (cdr lat)))))))


(define is-first?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (eq? a (car lat)))))

(define two-in-a-row-a?
  (lambda (lat)
    (cond ((null? lat) #f)
          (else (or (is-first? (car lat) (cdr lat))
                    (two-in-a-row? (cdr lat)))))))
;but is-first? may return #f because cdr lat is null
;if that's the case, it makes no sense to look futher

(define two-in-a-row?
  (lambda (lat)
    (cond ((null? lat) #f)
          (else (is-first-b? (car lat) (cdr lat))))))

(define is-first-b?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (or (eq? a (car lat)) (two-in-a-row? lat))))))

;but why use two functions, where one would suffice?
;two-in-a-row? calls is-first-b?, which calls two-in-a-row?, etc...
;you can recur with one function instead
(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond ((null? lat) #f)
          (else (or (eq? (car lat) preceding) (two-in-a-row-b? (car lat) (cdr lat)))))))

(define final-two-in-a-row?
  (lambda (lat)
    (cond ((null? lat) #f)
          (else (two-in-a-row-b? (car lat) (cdr lat))))))
;... I find it bizzare that they don't make it into a single function, like I did in my first take...

(define sum-of-prefixes
  (lambda (tup)
     (cond ((null? tup) '())
           (else (sum-of-prefixes-b 0 tup)))))

(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond ((null? tup) '())
          (else (cons (+ (car tup) sonssf)
                      (sum-of-prefixes-b (+ (car tup) sonssf) (cdr tup)))))))

(define pick
  (lambda (n lat)
    (cond ((= n 1) (car lat))
          (else (pick (- n 1) (cdr lat))))))

(define scramble-b
  (lambda (tup rev-pre)
    (cond ((null? tup) '())
          (else (cons (pick (car tup) (cons (car tup) rev-pre))
                      (scramble-b (cdr tup) (cons (car tup) rev-pre)))))))

(define scramble
  (lambda (tup)
    (scramble-b tup '())))
                       
(scramble '(1 1 1 3 4 2 1 1 9 2))
                        
                    