#lang racket

(define eqlist?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
          ((and (atom? (car l1)) (atom? (car l2))) (cond ((eq? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))
                                  (else #f)))
          ((or (atom? (car l1)) (atom? (car l2))) #f)
          (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(define (atom? x) (not (pair? x)))

(define find
  (lambda (n num-list res-list)
    (letrec ((F (lambda (n-list r-list)
                  (cond ((null? n-list) #f)
                        ((= n (car n-list)) (car r-list))
                        (else (F (cdr n-list) (cdr r-list)))))))
      (F num-list res-list))))

(define deep-1
  (lambda (m)
    (if (= m 0)
        'pizza
        (cons (deep-1 (- m 1)) '()))))

(define deepM-1
  (let ((results '())
        (numbers '()))
    (letrec ((D (lambda (m)
                  (if (= m 0)
                      'pizza
                      (cons (D (- m 1)) '())))))
      (lambda (n)
        (let ((existing-result (find n results numbers)))
          (if (atom? existing-result)
              (let ((result (D n)))
                (set! results (cons result results))
                (set! numbers (cons n numbers))
                result)
              existing-result))))))

;D is called only when the result is not already stored
;but D doesn't take advantage of the fact that there might be intermediate results stored, and it does unneccessary work

(define deepM-2
  (let ((results '())
        (numbers '()))
    (letrec ((D (lambda (m)
                  (if (= m 0)
                      'pizza
                      (cons (deepM-2 (- m 1)) '())))))
      (lambda (n)
        (let ((existing-result (find n results numbers)))
          (if (atom? existing-result)
              (let ((result (D n)))
                (set! results (cons result results))
                (set! numbers (cons n numbers))
                result)
              existing-result))))))
;(letrec...) is no onger neccessary for defining D "since D is no longer mentioned in the definition of D"
;we can simply use let
;apparently, letrec is neccessary only when writing recursive definitions
;...but I don't understand why...

(define deepM-3
  (let ((results '())
        (numbers '())
        (D (lambda (m)
              (if (= m 0)
                  'pizza
                  (cons (deepM-3 (- m 1)) '())))))
    (lambda (n)
      (let ((existing-result (find n results numbers)))
        (if (atom? existing-result)
            (let ((result (D n)))
              (set! results (cons result results))
              (set! numbers (cons n numbers))
              result)
            existing-result)))))
;but actually, D is used only once, so maybe there's no point naming it

(define deepM-4
  (let ((results '())
        (numbers '()))
    (lambda (n)
      (let ((existing-result (find n results numbers)))
        (if (atom? existing-result)
            (let ((result ((lambda (m)
                             (if (= m 0)
                                 'pizza
                                 (cons (deepM-4 (- m 1)) '())))
                           n)))
              (set! results (cons result results))
              (set! numbers (cons n numbers))
              result)
            existing-result)))))
;there is yet simpler phrasing

(define deepM-5
  (let ((results '())
        (numbers '()))
    (lambda (n)
      (let ((existing-result (find n results numbers)))
        (if (atom? existing-result)
            (let ((result (if (= n 0)
                              'pizza
                              (cons (deepM-5 (- n 1)) '()))))
              (set! results (cons result results))
              (set! numbers (cons n numbers))
              result)
            existing-result)))))

;ok, so deepM was written as a version of deep which saves work
;how much exactly does it help?

;for the purposes of establishing that, let's write a function which conses and keeps count of how many times it has been called
(define consC-1
  (let ((keep-count 0))
    (lambda (x l)
      (set! keep-count (+ keep-count 1))
      (cons x l))))

;now, if we use consC rather than cons in the definition of deep
(define deep-2
  (lambda (m)
    (if (= m 0)
        'pizza
        (consC-1 (deep-2 (- m 1)) '()))))

;but right now we have no way of accessing the value of the counter
;even though it counts how many times consC was used, there's no way to check it

(define counter (lambda () 0))

(define consC-2
  (let ((keep-count 0))
    (set! counter (lambda () keep-count)) ;why not simply (set! counter keep-count)?
    (lambda (x l)
      (set! keep-count (+ keep-count 1))
      (cons x l))))
;apparently SS wants counter to be a function of no arguments, rather than a value
;...that's strange, because if we call counter, it is supposed to return keep-count
;...but keep-count is not a public variable, but one that's internal to consC
;...so how can counter access it?...

;to get the number of times cons is called upon when evalauating deep for all n between 0 and 1000 we would need to
;call deep 1001 times
;... we might map deep over (enumerate-interval 0 1000)...
;or write supercounter
(define supercounter
  (lambda (f)
    (letrec ((S (lambda (n)
                  (if (= n 0)
                      (f n)
                      (let ()
                        (f n)
                        (S (- n 1)))))))
      (S 1000)
      (counter))))
;it's ok, but it's very specific right now, and exposed
;counter could be changed by anything at any time
;also, there's the implicit assumption that f is a function that resets counter
;...and it's so much more involved than map...

;(supercounter deep)
;(supercounter deep)
;500500
;1001000

;now SS wants to make sure that counter is reset when necessary
;curiously, my supercounter works fine as it is

(define set-counter (lambda (x) 0))

(define consC
  (let ((keep-count 0))
    (set! counter (lambda () keep-count))
    (set! set-counter (lambda (x) (set! keep-count x)))
    (lambda (x l)
      (set! keep-count (+ keep-count 1))
      (cons x l))))
;so, counter and set-counter are outside functions modified by consC
;keep-count is an internal variable
;counter stores the current value of keep-count
;and set-counter is a function which resets keep-count to a desired value
;...again, how can set-counter access the inner variable of consC?...

(define deep
  (lambda (m)
    (if (= m 0)
        'pizza
        (consC (deep (- m 1)) '()))))

(deep 5)
(deep 7)
(set-counter 0)
(supercounter deep)

;ok, so we know that cons is called upon 500500 times when using deep to on numbers from 0 to 1000
;what about deepM?
;let's make it use consC
(define deepM
  (let ((results '())
        (numbers '()))
    (lambda (n)
      (let ((existing-result (find n numbers results)))
        (if (atom? existing-result)
            (let ((result (if (= n 0)
                              'pizza
                              (consC (deepM (- n 1)) '()))))
              (set! results (cons result results))
              (set! numbers (cons n numbers))
              result)
            existing-result)))))

(set-counter 0)
(supercounter deepM)
;1000


;REMBER1* again
(define rember1*
  (lambda (a l)
    (letrec ((R (lambda (l oh)
                  (cond ((null? l) (oh 'no))
                        ((atom? (car l)) (if (eq? (car l) a)
                                             (cdr l)
                                             (consC (car l) (R (cdr l) oh))))
                        (else (let ((new-car (let/cc oh
                                               (R (car l) oh))))
                                (if (atom? new-car)
                                    (consC (car l) (R (cdr l) oh))
                                    (consC new-car (cdr l)))))))))
      (let ((new-l (let/cc oh (R l oh))))
        (if (atom? new-l)
            l
            new-l)))))

(set-counter 0)
(rember1* 'noodles '((food) more (food)))
(counter);it's 0, because rember1* forgot about all the conses when encountering oh

(define rember1*2
  (lambda (a l)
    (letrec ((R (lambda (l)
                  (cond ((null? l) '())
                        ((atom? (car l)) (if (eq? (car l) a)
                                             (cdr l)
                                             (consC (car l) (R (cdr l)))))
                        (else (let ((av (R (car l))))
                                (if (eqlist? (car l) av)
                                    (consC (car l) (R (cdr l)))
                                    (consC av (cdr l)))))))))
      (R l))))

(set-counter 0)
(rember1*2 'noodles '((food) more (food)))
(counter);it's 5, because rember1*2 doesn't use let/cc to return the answer immediately
