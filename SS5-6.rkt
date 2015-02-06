#lang racket


(define x (cons 'pizza '()))

(define dinerR
  (lambda (food)
    (set! x food)
    (cons 'milkshake (cons food '()))))

(define omnivore
  (let ((x 'minestrone))
    (lambda (food)
      (set! x food)
      (cons food (cons x '())))))

(define food 'none)
(define glutton
  (lambda (x)
    (set! food x)
    (cons 'more (cons x (cons 'more (cons x '()))))))

(define chez-nous
  (let ((temp food))
    (set! food x)
    (set! x temp)))
    
    
          

;Chapter 16
(define last 'anglefood)

(define sweet-toothL
  (lambda (food)
    (set! last food)
    (cons food (cons 'cake '()))))

(define ingredients '())
(define sweet-toothR
  (lambda (food)
    (set! ingredients (cons food ingredients))
    (cons food (cons 'cake '()))))

(define deep
  (lambda (m)
    (cond ((zero? m) 'pizza)
          (else (cons (deepM (- m 1)) '())))))

(define numbers '())
(define results '())

(define deepR
  (lambda (m)
    (let ((result (deep m)))
    (set! numbers (cons m numbers))
    (set! results (cons result results))
    result)))

(deepR 3)
;to use the fact that we're storing the results we need a way of checking whether we have already evaluated the function for a given argument
(define find
  (lambda (n num-list res-list)
    (letrec ((F (lambda (n-list r-list)
                  (cond ((null? n-list) #f)
                        ((= n (car n-list)) (car r-list))
                        (else (F (cdr n-list) (cdr r-list)))))))
      (F num-list res-list))))

;deepR unneccessarily conses numbers and results onto lists when they are already there
(define deepM-1
  (lambda (m)
    (if (member? m numbers)
        (find m numbers results)
        (let ((result (deep m)))
          (set! numbers (cons m numbers))
          (set! results (cons result results))
          result))))
;also, it's a shame that deep, which we're using here, doesn't take advantage of the fact that some results are already stored
;deep should recourse with deepM
;not only does it speed things up
;but each time deepM is called, the value of the result will be stored, even if we didn't explicitly asked for this value to be computed
;numbers and results shouldn't be structures independent of deepM

(define deepM
  (lambda (m)
    (let ((numbers '())
          (results '()))
      (let ((search-result (find m numbers results)))
        (if (atom? search-result);i.e if find returns #f
            (let ((result (deep m)))
              (set! numbers (cons m numbers))
              (set! results (cons result results))
              result)
            search-result)))))

(define length
  (let ((h (lambda (l) 0)))
    (set! h
          (lambda (l)
            (cond ((null? l) 0)
                  (else (+ (h (cdr l)) 1)))))
    h))

(define foo
  (let ((h (lambda (l) 0)))
    (set! h bar)
    h))
;where any one-argument function could be substituted for bar
;to get length, we could substitue e.g. this function for bar:
(define L
  (lambda (length)
    (lambda (l)
      (cond ((null? (l) 0))
            (else (+ (length (cdr l) 1)))))))

(define length
  (let ((h (lambda (l) 0)))
    (set! h (L (lambda (arg) (h arg)))); (lambda...) says simply "h"; it's like (set! h (L h))
    h))

;what is the value of (L (lambda (arg) (h arg)))?
;it's (lambda (length)
;      (lambda (l)
;        (cond ((null? (l) 0))
;              (else (+ (length (cdr l) 1))))))
;applied to (lambda (arg) (h (arg)))
;but we don't know the value of h
;precisely because h is this very function applied to this very argument
;"The value of h is the recursive function lenth"

(define Y!
  (lambda (L)
    (let ((h (lambda (l) '())))
      (set! h (L (lambda (arg) (h arg))))
      h)))

(define Y-bang
  (lambda (f)
    (letrec ((h (f (lambda (arg) (h arg)))))
      h)))

(define length (Y! L))

;...Ok, what's the point of it all???...