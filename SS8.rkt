#lang racket
(#%require (only r5rs set-cdr!))

;(define (kar x) (car x))
;(define (kdr x) (cdr x))
;(define (kons x y) (cons x y))

(define kounter (lambda () 0))
(define set-kounter (lambda (x) 0))
(define konsC
  (let ((keep-count 0))
    (set! kounter (lambda () keep-count))
    (set! set-kounter (lambda (x) (set! keep-count x)))
    (lambda (x l)
      (set! keep-count (+ keep-count 1))
      (kons x l))))

(define lots
  (lambda (n)
    (cond ((= n 0) '())
          (else (kons 'egg (lots (- n 1)))))))

(define lenkth
  (lambda (l)
    (cond ((null? l) 0)
          (else (+ (lenkth (kdr l)) 1)))))

(define add-at-end
  (lambda (l)
    (cond ((null? (kdr l)) (konsC (kar l) (kons 'egg '())))
          (else (konsC (kar l) (add-at-end (kdr l)))))))

;(add-at-end (lots 3))
;(kounter);3
;how to add at the end by using only one kons?

(define add-at-end-too
  (lambda (l)
    (letrec ((A (lambda (ls)
                  (cond ((null? (kdr ls)) (set-cdr! ls (kons 'egg '())))
                        (else (A (kdr ls)))))))
      (A l)
      l)))
;we do not strip the list of cars
;instead we find the very last cdr and change it, without doing anything else to the rest of the list

(define kons-1
  (lambda (kar kdr)
    (lambda (selector)
      (selector kar kdr))))

(define kar-1
  (lambda (c)
    (c (lambda (a d) a))))

(define kdr-1
  (lambda (c)
    (c (lambda (a d) d))))

(define bons
  (lambda (kar)
    (let ((kdr '()))
      (lambda (selector)
        (selector (lambda (x) (set! kdr x))
                  kar
                  kdr)))))

(define kar
  (lambda (c)
    (c (lambda (s a d) a))))

(define kdr
  (lambda (c)
    (c (lambda (s a d) d))))

(define x (bons 'egg))
;when we evaluate (bons a), we create a new internal variable, kdr
;its value can change
(define set-kdr
  (lambda (c x)
    ((c (lambda (s a d) s)) x)))

(define kons
  (lambda (a b)
    (set-kdr (bons a) b)
    (bons a)))

(define dozen (lots 12))
;it requires 12 conses
(define bakers-dozen (add-at-end dozen))
;it requires 13 conses- go through "dozen", separating subsequent cars and remembering to cons them back once we've added egg at the end
(define bakers-dozen-too (add-at-end-too dozen))
;this requires one cons

;"the konses in dozen are the same as first 12 in bakers-dozen-too"
;but the first 12 konses in bakers-dozen are not the same ones as dozen
;we take dozen, and separate its elements one by one, and then reasemble them
;still, despite the fact that the conses are not "the same" in some way, bakers-dozen and bakers-dozen-too are eqaual
;they are equal as values despite being constructed differently

;once we introduce assignment we can talk of sameness which is different from equality of values
;two things are the same if changing one changes another

(define same?
  (lambda (c1 c2)
    (let ((t1 (kdr c1))
          (t2 (kdr c2)))
      (set-kdr c1 1)
      (set-kdr c2 2)
      (let ((v (= (kdr c1) (kdr c2))))
        (set-kdr c1 t1)
        (set-kdr c2 t2)
        v))))

;(same? bakers-dozen bakers-dozen-too)

(define last-kons
  (lambda (ls)
    (cond ((null? (kdr ls)) ls)
          (else (last-kons (kdr ls))))))


;more on sameness:
;If you set the end of the list to be some earlier point in the same list
;the list will no longer have an end
;and if you e.g. try to check it's length, you'll never get an answer
;so maybe let's write a function which can deaf with intinite lists
(define finite-lenkth
  (lambda (p)
    (let/cc infinite
      (letrec ((C (lambda (p q)
                    (cond ((same? p q) (infinite #f))
                          ((null? q) 0)
                          ((null? (kdr q)) 1)
                          (else (+ (C (sl p) (qk q)) 2)))))
               (qk (lambda (x) (kdr (kdr x))))
               (sl (lambda (x) (kdr x))))
        (cond ((null? p) 0)
              (else (+ (C p (kdr p)) 1)))))))
;ok, let's say (finite-lenkth '(1 2 3))
;(+ (C (1 2 3) (2 3)) 1)
;(+ (+ (C (2 3) '()) 2) 1)
;(+ (+ 0 1) 2)

;how about an infinite list, say
;(define l '(1 2 3))
;(define in-l (set! (last-kons l) l))
;(+ (C (1 2 3 1 2 3 1 2 3 ... ) (2 3 1 2 3 1 2 3 ...)) 1)
;(+ (+ (C (2 3 1 2 3 1 2 3...) (1 2 3 1 2 3 1 2 3 ...)) 2) 1)
;(+ (+ (+ (C (3 1 2 3 1 2 ...) (3 1 2 3 1 2 3 1 2 ...)) 2) 2) 1)
;now (same? p q) is #t
;so we forget all about the additions saved for later, and return #f

;the whole point is to change p and q in different paces, so that they align at some point
;... but I don't get why couldn't we just not change p at all
;... is a list is infinite because of auto-assignment, q will align with p sooner if we don't change p in consecutive calls to C
;... then q could change by 1, i.e. (kdr q) -  it'd be slower, but we would never miss the aligning point
;... oh, ok, now I see. We don't need to loop over the whole list, it might be a smaller part of l that is set to be last-kons of l
;... so p needs to change on subsequent calls to C
                    