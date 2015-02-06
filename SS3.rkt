#lang racket

(define member?
  (lambda (a l)
    ((letrec ((m (lambda (l)
                  (cond ((null? l) #f)
                        ((eq? (car l) a) #t)
                        (else (m (cdr l)))))))
     m)
    l)))

(define intersect-1
  (lambda (set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((member? (car set1) set2) (cons (car set1) (intersect-1 (cdr set1) set2)))
          (else (intersect-1 (cdr set1) set2)))))

(define intersect
  (lambda (set1 set2)
    (letrec ((I (lambda (set)
                  (cond ((null? set) '())
                        ((member? (car set) set2) (cons (car set) (I (cdr set))))
                        (else (I (cdr set)))))))
      (I set1))))

;there was also a function for intersecting a list of sets
(define intersectall-1
  (lambda (lset)
    (cond ((null? (cdr lset)) (car lset))
          (else (intersect (car lset) (intersectall-1 (cdr lset)))))))
;it doesn't ask (null? lset), beacue an assumption was that the list is not empty
;what if we make no assumptions?
;we can just add ((null? lset) '()) clause
;but that's a question that we need only ask once
;once we know that lset is not empty, subsequent calls to intersectall will spuriously check if lset is empty and procede to other clauses
;maybe there's a way to ask this question once only, and then procede to use the function as it's written above

(define intersectall-2
  (lambda (lset)
    (letrec ((IA (lambda (lset)
                   (cond ((null? (cdr lset)) (car lset))
                         (else (intersect (car lset) (IA (cdr lset))))))))
      (cond ((null? lset) '())
            (else (IA lset))))))
;procedure recourses with the auxiliary function once we've made sure that lset is not empty

;when lset contains (), intersectall doesn't immediately return ()
;it keeps queueing intersect operations untill it reaches the end of lset, and then it evecutes all of these operations before returning ()
(define intersectall-3
  (lambda (lset)
    (let/cc hop
    (letrec ((IA (lambda (lset)
                   (cond ((null? (car lset)) (hop '()))
                         ((null? (cdr lset)) (car lset))
                         (else (intersect (car lset) (IA (cdr lset))))))))
      (cond ((null? lset) '())
            (else (IA lset)))))))
;hop is used in a condition as if it was a function
;what happens once the answer to (null? (car lset)) is #t is this:
;forget everything that happened in between let/cc and the current point
;and just determine the value of whatever's hop's argument

;yeah, right now we come to the conclusion that intersect should also check whether it's second argument is empty
;otherwise we might end up with a long queue of intersect operations to perform
;first of which returns ()
;but we go through all of them anyway, because intersect (as implemented by SS) doesn't have a condition for (null? set2)
;to ameliorate, let's make intersect an internal function of intersectall

(define intersectall
  (lambda (lset)
    (let/cc hop
      (letrec ((IA (lambda (lset)
                     (cond ((null? (car lset)) (hop '()))
                           ((null? (cdr lset)) (car lset))
                           (else (I (car lset) (IA (cdr lset)))))))
               (I (lambda (set1 set2)
                    (letrec ((i (lambda (set)
                                  (cond ((null? set) '())
                                        ((member? (car set) set2) (cons (car set) (I (cdr set) set2)))
                                        (else (I (cdr set1)))))))
                      (cond ((null? set2) (hop '()));why not (hop '())- that's what I wrote at first ... in the end they do put hop in
                            (else (i set1)))))))
        (cond ((null? lset) '())
              (else (IA lset)))))))


(define rember
  (lambda (a lat)
    (letrec ((R (lambda (lat)
                  (cond ((null? lat) '())
                        ((eq? (car lat) a) (cdr lat))
                        (else (cons (car lat) (R (cdr lat))))))))
      (R lat))))

(define rember-beyond-first
  (lambda (a lat)
    (letrec ((R (lambda (lat)
                  (cond ((null? lat) '())
                        ((eq? (car lat) a) '())
                        (else (cons (car lat) (R (cdr lat))))))))
      (R lat))))

;rember-upto-last goes through a lat and forgets everything up to and including the last occurence of a
;so, upon not seeing a we need to cons (car lat) onto the output, because it's possible we won't see a at all till the end of the list
;but if we do chance upon a at a later point, we need to forget all about the consing that we have already scheduled
;and start anew with the rest of the list, if there's any
;in intersectall we know what was the restult once a given condition was met - we hopped to ()
;now we also want to hop, but the result is not simply a value, but an expression to be evaluated

(define rember-upto-last
  (lambda (a lat)
    (let/cc skip
    (letrec ((R (lambda (lat)
                  (cond ((null? lat) '())
                        ((eq? (car lat) a) (skip (R (cdr lat)))) ;this says evaluate (let/cc skip (R (cdr lat)) and forget what was done earlier
                        (else (cons (car lat) (R (cdr lat))))))))
      (R lat)))))

