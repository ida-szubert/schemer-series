#lang racket
(define (atom? x) (not (pair? x)))

(define deep
  (lambda (m)
    (cond ((= m 0) 'pizza)
          (else (cons (deep (- m 1)) '())))))

(define toppings '())

(define deepB
  (lambda (m)
    (cond ((= m 0) (let/cc jump
                     (set! toppings jump)
                     'pizza))
          (else (cons (deepB (- m 1)) '())))))

;the value part of let/cc has two parts
;and toppings seems not to be used at all, so why set it to anything, let alone to the let/cc special word
;jump also isn't used the way hop or skip have been used previously

;when we evaluate (deepB 6), we get ((((((pizza)))))), and when m=0, we also set toppings to jump
;and now if we evaluated (toppings 'cake), we'd get ((((((cake))))))
;as if jump was a function which send us to the point where we were about to cons pizza onto 6 '(), only that instead of pizza it uses toppings' arg
;jump somehow is a function which conses sth onto the number of () most recently asked of deepB
;...but how???...

;now, what if we said (cons (toppings 'cake) '())
;we expect cake consed onto 7 ()
;but it won't happen; toppings uses let/cc special word, so it forgets whatever surrounds it
;whenever we use a value made with (let/cc...), it forgets everything around it

(define deep&co
  (lambda (m k)
    (cond ((= m 0) (k 'pizza))
          (else (deep&co (- m 1)
                         (lambda (x) (k (cons x '()))))))))

;in general, a collector (aka continuation) is a function which specifies what happens later with the result of some other function
;the second argument of deep&co, on any interation, is a function which specifies onto how many ()-s will a given atom get consed
;initially k must be the identity function, returning x when given x

;(deep&co 0 (lambda (x) x))
;pizza

;we stop at the first clause

;(deep&co 2 (lambda (x) x))
;(deep&co 1 (lambda (x) ((lambda (x) x) (cons x '())))), which is (deep&co 1 (lambda (x) (cons x '())))
;(deep&co 0 (lambda (x) ((lambda (x) (cons x '())) (cons x '()))))
;((lambda (x) ((lambda (x) (cons x '())) (cons x '()))) 'pizza)
;((lambda (x) (cons x '())) '(pizza))
;((pizza))
;so, k is always a function which conses x onto the requested number of ()-s 

;... k seems not to do anything usefull, as a matter of fact...
;... also, why should k be a variable that the user needs to supply? we could have an internal recursive function, doing all the work
;... which knows that it needs to start with (lambda (x) x) for k
;deep&co is a version of deep which uses a collector
;the point seems to be that on the last call to deep&co, k will be a function which conses 'pizza onto all the required ()-s
;it'll be like one of these 4-layers, 6-layers, etc. functions at the beginning of the chapter


;so SS ask whether there's a better way to describe the collector
;we're still talking about the final collector of (deep&co 2 (lambda (x) x))
(define  two-layers
  (lambda (p)
    (cons (cons p '()) '())))
;well, that's more concise, without all the lambdas, but it's essentially the same
;for (deep&co 6 (lambda ...)) the last collector would be six-layers
;we can actually remember consecutive colletors in a separate function outside of deep&co
;... that's not to say that I see why that would be beneficial...

(define deep&coB
  (lambda (m k)
    (cond ((= m 0) (let ()
                     (set! toppings k)
                     (k 'pizza)))
          (else (deep&coB (- m 1)
                          (lambda (x) (k (cons x '()))))))))
;it also has a collector, but its current value is on each iteration stored in toppings
;after evaluationg (deep&coB 2 identity), toppings will be (lambda (x) (cons (cons x'()) '()))
;but - there's a difference between deepB and deep&coB
;deep&coB sets toppings to a particular function directly, while deepB used let/cc
;it means that now toppings doesn't ignore all of the surrounding instructions, it acts just like a normal function


;TWO-IN-A-ROW, WALK, and WADDLE
(define two-in-a-row?
  (letrec ((W (lambda (a lat)
                (cond ((null? lat) #f)
                      (else (let ((next (car lat)))
                              (or (eq? next a) (W next (cdr lat)))))))))
    (lambda (lat)
      (cond ((null? lat) #f)
            (else W (car lat) (cdr lat))))))

;we want two-in-a-row*?, which checks for an atom occuring twice in a row, disregading any parentheses
;we could use two-in-a-row? if we had a way of flattening the list
(define leave '())

(define walk
  (lambda (l)
    (cond ((null? l) '())
          ((atom? car l) (leave (car l)))
          (else (let ()
                  (walk (car l))
                  (walk (cdr l)))))))
(define start-it
  (lambda (l)
    (let/cc here
      (set! leave here)
      (walk l))))
;so, when walk finds an atom, it becomes the designated value of leave, which is also the value of here
;we can find and return the leftmost atom in a list
;start-it returns the first atom in a list
;... why couldn't we use (let/cc ...) inside start-it, the way we used hop or skip?...
;... start it does nothing else byt sets leave to be a compas needle, and initiales walk

    ;I GET IT, or at least I get some of what's going on
    ;(let/cc...) used in start establishes a continuation that is empty, i.e there's nothing to be done to whatever value leave is applied to
    ;(let/cc in waddle captures a continuation which occurs in the middle of evaluating waddle, and specifies that (waddle (cdr l)) needs to be evaluated
    ;when we encounter leave in waddle, start-it2 returns (car l), because the continuation captured by heare is empty
    ;fill stores the continuation captured before returning (car l)
    ;when we want to get the next element, we instruct fill to evaluate the captured continuation
    ;OMG, it finally makes some sense!

(define fill '())

(define waddle
  (lambda (l)
    (cond ((null? l) '())
          ((atom? (car l)) (let ()
                             (let/cc rest
                               (set! fill rest)
                               (leave (car l)))
                             (waddle (cdr l))))
          (else (let ()
                  (waddle (car l))
                  (waddle (cdr l)))))))
;if (leave (car l)) ever has a value, waddle should look at the elements in (cdr l)
;that's strange, because leave always forgets anyway, so (waddle (cdr l)) won't get evaluated
;it makes sense (presumably) because before evaluating (leave (car l)) and forgeting everything else, waddle remembers what's left to do in fill
;(leave (car l)) still doesn't yeld a value, so how would (waddle (cdr l)) ever get evaluated?
;but if fill is called, it will remember to evaluate (waddle (cdr l))
;... why should it? we said that (let() a b) b is evaluated if a has a value
;... why should it be that calling fill, which amounts to calling rest, should count as the whole (let/cc ...) expression having a value?...


;... that's strange...
;(leave (car l)) will never have a value
;but (waddle (cdr l)) will be called whenever fill is used

(define start-it2
  (lambda (l)
    (let/cc here
      (set! leave here)
      (waddle l))))
;so, when calling start-it2 we'll get the first atom as a result, and fill will be set to rest
;rest is a compas needle
;... does it mean that fill kind of comes back to the point in the evaluation when the firt atom was already found and returned,
;... and we're about to look at (cdr l)?...
;like jump in deepB, rest is sort of a function

;taht;s whatrest corresponds to after one call to waddle:
;(define rest1
;  (lambda (x)
;    (waddle l1)))
;where l1 is the rest of the list after the first atom atom has been found

(define get-next
  (lambda (x)
    (let/cc here-again
      (set! leave here-again)
      (fill 'go))))
;calling (fill 'go) is in effect like calling (rest 'go)
;get-next causes the rest of l to be inspected for the leftmost atom
;it works because leave is set to here-again, and when (levale 'cherioos) is evaluated, cherioos is returned by get-next
;also, by the end of evaluating get-next, fill would remember a new rest of list
;fill effectively changes itself (because waddle sets fill to a new value) every time it's called

(start-it2 '((donuts) (cherios (cherios (spaghettios))) donuts))
(get-next 'go)
(get-next 'go)
(get-next 'go)
(get-next 'go)
(get-next 'go)

;now, what happens when the remembered rest is ()?
;waddle returns ()
;so does start-it-too
;... but why? we were using get-next all the time
;... and doesn't get-next return () when rest is ()? It should, because fill is set to rest-n, and rest-n returns ()
;... and it does. WTF, Seasoned Schemer???
;... how can start-it2 "get the first atom, but later it also returns () when everything is over"
;... that is, when and how does it return ()? Because I haven't seen it happening

;... I still think rest is really strange
;... it's supposed to be a function, but there's nothing in waddle that tells me that rest is a function

;ok, so SS says that waddle should return the last () using (leave '()), so that get-next will return () when asked for the next atom and 
;the list is already exhausted
;since they say that start-it2 returns () when everything is over, they give another function solely for getting the first atom out.

(define get-first
  (lambda (l)
    (let/cc here
      (set! leave here)
      (waddle l)
      (leave '()))))

(get-first '(fish (chips)))
(get-next 'go)
(get-next 'go)

;now, we use get-first and get-next to define two-in-a-row*?
(define two-in-a-row*?-1
  (lambda (l)
    (let ((first (get-first l)))
      (if (atom? first)
          (two-in-a-row-b*? first)
          #f))));this means that there are no more elements in l and the value of first in ()

(define two-in-a-row-b*?
  (lambda (a)
    (let ((next (get-next 'go)))
      (if (atom? next)
          (or (eq? a next) (two-in-a-row-b*? next))
          #f))))

;... I'm not sure how good an idea is it to write function which seem to take arguments "from thin air"
;... you cannot say from the definition that two-in-a-row-b*? operates on a list, it gets the next element of the list in a convoluted way

(define two-in-a-row*?
  (letrec
      ((test? (lambda (a)
                (let ((next (get-next 'go)))
                  (if (atom? next)
                      (or (eq? a next) (test? next))
                      #f))))
       (get-next (lambda (x)
                   (let/cc here-again
                     (set! leave here-again)
                     (fill 'go))))
       (fill (lambda (x) x))
       (waddle (lambda (l)
                 (cond ((null? l) '())
                       ((atom? (car l)) (let ()
                                          (let/cc rest
                                            (set! fill rest)
                                            (leave (car l)))
                                          (waddle (cdr l))))
                       (else (let ()
                               (waddle (car l))
                               (waddle (cdr l))))))))
    (lambda (l)
      (let ((first (let/cc here
                     (set! leave here)
                     (waddle l)
                     (leave '()))))
        (if (atom? first)
            (test? first)
            #f)))))
;that's all in one package
;fist is used once only, so there's no internal definition

;I still think it's a bit crazy

       

