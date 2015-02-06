#lang racket

(define (atom? x) (not (pair? x)))

(define leftmost-1
  (lambda (l)
    (cond ((atom? (car l)) (car l))
          (else (leftmost-1 (car l))))))

;it doesn't work if the leftmost thing is ()
;when writing leftmost, the assumption was that l is a non-empty list which doesn't contain ()
;we want it to work on lists in which () is at the leftmost place
;but we don't want leftmost to return () or say that there's no leftmost atom
;there is one, it just doesn't occupy the leftomst position as we've defined it

(define leftmost-2
  (lambda (l)
    (cond ((null? l) '())
          ((atom? (car l)) (car l))
          (else (let ((a (leftmost-2 (car l))))
                  (cond ((atom? a) a)
                        (else (leftmost-2 (cdr l)))))))))


(define first-rember1*
  (lambda (a l)
    (cond ((null? l) '())
          ((atom? (car l)) (cond ((eq? (car l) a) (cdr l))
                                 (else (cons (car l) (first-rember1* a (cdr l))))))
          (else (cond ((eqlist? (first-rember1* a (car l)) (car l)) (cons (car l) (first-rember1* a (cdr l))))
                      (else (cons (first-rember1* a (car l)) (cdr l))))))))

(define second-rember1*
  (lambda (a l)
    (letrec ((R (lambda (l)
                  (cond ((null? l) '())
                        ((atom? (car l)) (cond ((eq? (car l) a) (cdr l))
                                               (else (cons (car l) (R (cdr l))))))
                        (else (cond ((eqlist? (R (car l)) (car l)) (cons (car l) (R (cdr l))))
                                    (else (cons (R (car l)) (cdr l)))))))))
      (R l))))
;rember1* removes the leftmost occurence of a from a list
(define third-rember1*
  (lambda (a l)
    (letrec ((R (lambda (l)
                  (cond ((null? l) '())
                        ((atom? (car l)) (cond ((eq? (car l) a) (cdr l))
                                               (else (cons (car l) (R (cdr l))))))
                        (else (let ((av (R (car l))))
                                (cond ((eqlist? av (car l)) (cons (car l) (R (cdr l))))
                                    (else (cons av (cdr l))))))))))
      (R l))))
;..well, let helps a bit with readability, but why does (R (car l)) used twice "seem wrong"?...

(define eqlist?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
          ((and (atom? (car l1)) (atom? (car l2))) (cond ((eq? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))
                                  (else #f)))
          ((or (atom? (car l1)) (atom? (car l2))) #f)
          (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))
;that's just for testing rember1*

(define depth*-1
  (lambda (l)
    (cond ((null? l) 1)
          ((atom? (car l)) (depth*-1 (cdr l)))
          (else (cond ((> (depth*-1 (cdr l)) (+ (depth*-1 (car l)) 1))
                       (depth*-1 (cdr l)))
                      (else (+ (depth*-1 (car l) 1))))))))

(define depth*-2
  (lambda (l)
    (let ((a (+ (depth*-2 (car l)) 1))
          (d (depth*-2 (cdr l))))
      (cond ((null? l) 1)
            ((atom? (car l)) d)
            (else (cond ((> d a) d)
                        (else a)))))))

;oh, it doesn't work for an interesting reason
;we start with determining the value of a
;which is (+ (depth* ()) 1), so we look for depth* ()
;which means we need to determine the value of a
;but there's isn't any (car ())
;so, let should be placed somewhere else, definitely after the ((null? l) 1) condition
(define depth*-3
  (lambda (l)
    (cond ((null? l) 1)
          ((atom? (car l)) (depth*-3 (cdr l)))
          (else
           (let ((a (+ (depth*-3 (car l)) 1))
                 (d (depth*-3 (cdr l))))
             (cond ((> d a) d)
                   (else a)))))))

(depth*-3 '(() ((bitter butter) (makes) (batter (bitter))) butter))
;4

;depth* would be clearer if we used (if (> d a) d a)
;also, we might use a function max to choose the higher of two numbers

(define max
  (lambda (n m)
    (if (> n m) n m)))

(define depth*
  (lambda (l)
    (cond ((null? l) 1)
          ((atom? (car l)) (depth* (cdr l)))
          (else (max (+ (depth* (car l)) 1)
                     (depth* (cdr l)))))))
;let is unnecessary, since we only use the relevant expressions once

(define scramble
  (lambda (tup)
    (letrec ((pick (lambda (n lat)
                     (cond ((= n 1) (car lat))
                           (else (pick (- n 1) (cdr lat))))))
             (P (lambda (tup rp)
                  (cond ((null? tup) '())
                        (else (let ((new-rp (cons (car tup) rp)))
                                (cons (pick (car tup) new-rp)
                                      (P (cdr tup) new-rp))))))))
      (P tup '()))))

;let's go back to leftmost
;(leftomst '(((x)) y (z)))
;the list isn't empty and it's car isn't an atom
;in fact, we make detours in evaluation every time we evaluate a
;so on the first go we say: let a be (leftmost ((x)))
;and we go to evaluate it, which leads us to saying: let a be (leftmost (x))
;we go and evaluate it, and get an answer
;so we substitute this answer for a, and carry on with the evaluation
;we ask: ataom? a, and x is an atom, so we return x
;and substitue it for a one level up
;and carry on again, asking atom? x and returning x

;that's a lot of work done since we actually had the final answer

(define leftmost-3
  (lambda (l)
    (let/cc skip
      (lm l skip))))

(define lm
  (lambda (l out)
    (cond ((null? '()))
          ((atom? (car l) (out (car l))))
          (else (let ()
                  (lm (car l) out)
                  (lm (cdr l) out))))))

;that's a strange let
;it has two expressions in the value part
;we first determine the value of the first one. If it has a value, we ignore it and go to determine the value of the second expression
;the trick is that once we get to x, which is the answer, we are remembering to do two things
;to calculate (lm () out) and (lm (y (z)) out)
;but when x is found, we evaluate (skip x), which says: forget all you remembered to do, and just return x
;and we're done
;but it can be written better
(define leftomost-4
  (lambda (l)
    (let/cc skip
      (letrec ((lm (lambda (l out)
                     (cond ((null? '()))
                           ((atom? (car l) (out (car l))))
                           (else (let ()
                                   (lm (car l) out)
                                   (lm (cdr l) out)))))))
        (lm l skip)))))
;also, out will always be skip, so why not use this fact to simplify?
(define leftomost
  (lambda (l)
    (let/cc skip
      (letrec ((lm (lambda (l)
                     (cond ((null? '()))
                           ((atom? (car l) (skip (car l))))
                           (else (let ()
                                   (lm (car l))
                                   (lm (cdr l))))))))
        (lm l)))))
;that's a nice way to write a function which recursively inspects elements of a list and prompty returns the answer
;right away when it's found, rather then passing the answer back untill it reaches the first call to the function

;now onto rember1*
(define fourth-rember1*
  (lambda (a l)
    (letrec ((R (lambda (l)
                  (cond ((null? l) '())
                        ((atom? (car l)) (cond ((eq? (car l) a) (cdr l))
                                               (else (cons (car l) (R (cdr l))))))
                        (else (let ((av (R (car l))))
                                (cond ((eqlist? av (car l)) (cons (car l) (R (cdr l))))
                                    (else (cons av (cdr l))))))))))
      (R l))))
;how could let/cc be useful in rember1*?
;"we could abruptly signal that the list in the (car l) didn't contain the interesting atom"
(define rm-1
  (lambda (a l oh)
    (cond ((null? l) (oh 'no))
          ((atom? (car l)) (if (eq? (car l) a)
                               (cdr l)
                               (cons (car l) (rm-1 a (cdr l) oh))))
          (else ;(car l) is a list; if it doesn't contain a, (rm (car l)) returns the atom no, otherwise it returns a list
           (if (atom? (let/cc oh
                        (rm-1 a (car l) oh)))
               (cons (car l) (rm-1 a (cdr l) oh))
               (cons (rm-1 a (car l) 0) (cdr l)))))));there's no oh because we simply want to reproduce the value we've checke earlier
               
(define 5-rember1*
  (lambda (a l)
    (let ((new-l (let/cc oh (rm-1 a l oh)))) 
      (if (atom? new-l); it will be the atom "no" if a isn't found anywhere in l
          l
          new-l))))

;rm could use some let as well
(define rm-2
  (lambda (a l oh)
    (cond ((null? l) (oh 'no))
          ((atom? (car l)) (if (eq? (car l) a)
                               (cdr l)
                               (cons (car l) (rm-2 a (cdr l) oh))))
          (else ;(car l) is a list; if it doesn't contain a, (rm (car l)) returns the atom no, otherwise it returns a list
           (let ((new-car (let/cc oh
                            (rm-2 a (car l) oh))))
             (if (atom? new-car)
                 (cons (car l) (rm-2 a (cdr l) oh))
                 (cons new-car (cdr l))))))))


;TRY
(define-syntax try 
  (syntax-rules () 
    ((try var a . b) 
     (let/cc success 
       (let/cc var (success a)) . b))))

(define rember1*
  (lambda (a l)
    (try oh (rm-2 a l oh) l)))

(define rm
   (lambda (a l oh)
    (cond ((null? l) (oh 'no))
          ((atom? (car l)) (if (eq? (car l) a)
                               (cdr l)
                               (cons (car l) (rm-2 a (cdr l) oh))))
          (else
           (try oh2
                (cons (rm a (car l) oh2)
                          (cdr l))
                (cons (car l) (rm a (cdr l) oh)))))))

