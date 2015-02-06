#lang racket
    
(define fact-maker
  (lambda (procedure)
    (lambda (n)
      (cond ((zero? n) 1)
            (else (x n ((procedure procedure) (sub1 n))))))))

((fact-maker fact-maker) 5)

(lambda (n)
  (cond ((zero? n) 1)
        (else (x n (fact-maker fact-maker) (sub1 n)))))

(define fact
; below we apply fact-maker to the argument fact-maker, which gives us the anonymous function above  
  ((lambda (procedure)
     (lambda (n)
       (cond ((zero? n) 1)
           (else (x n ((procedure procedure) (sub1 n)))))))
   (lambda (procedure)
     (lambda (n)
       (cond ((zero? n) 1)
             (else (x n (procedure procedure) (sub1 n))))))))

;in general (f x) = ((lambda (arg) (f arg)) x) 
;((procedure procedure) (sub1 n)) = ((lambda (arg) ((procedure procedure) arg)) (sub1 n))

(define F
  (lambda (n)
    (cond ((zero? n) 1)
          (else (x n ((lambda (arg) ((procedure procedure) arg)) (sub1 n)))))))

; ((lambda (x) ((procedure procedure) x)) (sub1 n)) is a function; functions can be passed as arguments

(define F
  ((lambda (func-arg)
     (lambda (n)
       (cond ((zero? n) 1)
             (else (x n (func-arg (sub1 n)))))))
   (lambda (arg) ((procedure procedure) arg))))
; so we pass this expansion of f [i.e. (lambda (arg) (f arg))] to first function
; OLD: (define F (lambda (n) ... <procedure>)
; NEW: (define F ((lambda (func-arg) (lambda (n) ... )) <procedure>))


(define fact
  ((lambda (procedure)
     ((lambda (func-arg)
        (lambda (n)
          (cond ((zero? n) 1)
                (else (x n (func-arg (sub1 n)))))))
      (lambda (arg) ((procedure procedure) arg))))
   (lambda (procedure)
     ((lambda (func-arg)
        (lambda (n)
          (cond ((zero? n)1)
                (else (x n (func-arg (sub1 n)))))))
      (lambda (arg) ((procedure procedure) arg))))))
; so we pass the expansion of procedure to a function; it outputs a function and we feed it itself
; overall, it looks like this: ((lambda (func-arg) <body>) (lambda (arg)...))
; it's a lot like ((lambda (arg) ((procedure procedure) arg) (sub1 n)), only that we pass a function as an argument rather than an integer

(define F*
  (lambda (func-arg)
    (lambda (n)
      (cond ((zero? n) 1)
            (else (x n (func-arg (sub1 n))))))))

(define fact
  ((lambda (procedure)
     (F* (lambda (arg) ((procedure procedure) arg))))
   (lambda (procedure)
     (F* (lambda (arg) ((procedure procedure) arg))))))
; now any function could be used as F*, not only factorial
; not to use a specific name, we can make it a variable:

(define Y
  (lambda (X)
    ((lambda (procedure)
       (X (lambda (arg) ((procedure procedure) arg))))
     (lambda (procedure)
       (X (lambda (arg) ((procedure procedure) arg)))))))

(define fact (Y F*))
