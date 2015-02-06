#lang racket

(define add1
  (lambda (n)
    (+ 1 n)))

(define <
  (lambda (n m)
    (cond ((zero? m) #f)
          ((zero? n) #t)
          (else (< (sub1 n) (sub1 m))))))

(define >
  (lambda (n m)
    (cond ((zero? n) #f)
          ((zero? m) #t)
          (else (> (sub1 n) (sub1 m))))))

(define =
  (lambda (n m)
    (cond ((> n m) #f)
          ((< n m) #f)
          (else #t))))
                     
(define %
  (lambda (n m)
    (cond ((< n m) 0)
          (else (add1 (% (o- n m) m))))))

(define o-
  (lambda (n m)
    (cond ((zero? m) n)
          (else (sub1 (o- n (sub1 m)))))))


(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((number? (car lat)) (no-nums (cdr lat)))
              (else (cons (car lat) (no-nums (cdr lat)))))))))


(no-nums '(forget 8 me 2 7 not))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
              (else (all-nums (cdr lat))))))))

(all-nums '(forget 6 me 2 3 not))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f) 
      (else (eq? a1 a2)))))

(eqan? 2 2)
(eqan? 'cat 'food)
(eqan? 'oink 'oink)


(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else (cond
              ((eq? (car lat) a) (add1 (occur a (cdr lat))))
              (else (occur a (cdr lat))))))))

(occur 5 '(6 7 8 9 5 5))

(define one?
  (lambda (n)
    (= n 1)))

(one? 1)
(one? 6)

(define rempick
  (lambda (n lat)
    (cond ((null? lat) '())
          ((one? n) (cdr lat))
          (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick 2 '(very nasty sweet kittens)) 

(define (atom? x) (not (pair? x))) 

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (cond
                         ((eq? (car l) a) (rember* a (cdr l)))
                         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l) (cond
                        ((eq? (car l) old) (cons (car l) (cons (new (cdr l)))))
                        (else (cons (car l) (insertR* new old (cdr l)))))))
      (else (cons insertR* new old (car l) (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l)) (cond
                        ((eq? (car l) a) (add1 (occur* a (cdr l))))
                        (else (occur* a (cdr l)))))
      (else (o+(occur* a (car l)) (occur* a (cdr l)))))))

(define o+
  (lambda (n m)
    (cond ((zero? m) n)
          (else (add1 (o+ n (sub1 m)))))))

(occur* 'banana '(banana '(guards of '(banana kingdom))))

(define subst*
  (lambda (new old l)
    (cond ((null? l) '())
          ((atom? (car l)) (cond
                             ((eq? (car l) old) (cons new (subst* new old (cdr l))))
                             (else (cons (car l) (subst* new old (cdr l))))))
          (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond ((null? l) '())
          ((atom? (car l)) (cond
                             ((eq? (car l) old) (cons new (cons (car l) (insertL* new old (cdr l)))))
                             (else (cons (car l) (insertL* new old (cdr l))))))
          (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))
                          
(insertL* 'banana 'split '(can '(I have) '(this lovely '(split '(to split with) you))))


(define member*
  (lambda (a l)
    (cond ((null? l) #f)
          ((atom? (car l)) (or ((eq? (car l) a))  (member* (cdr l))))
          (else (or (member* (car l)) (member* (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond ((atom? (car l)) (car l))
          (else (leftmost (car l))))))

(define eqlist?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
          ((or (null? l1) (null? l2)) #f)
          ((and (atom? (car l1)) (atom? (car l2))) (cond
                                                     ((eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))
                                                     (else #f)))
          ((or (atom? (car l1)) (atom? (car l2))) #f)
          (else (cond
                  ((eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))
                  (else #f))))))

(define equal?
  (lambda (s1 s2)
    (cond  ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
           ((or (atom? s1) (atom? s2)) #f)
           (else (eqlist? s1 s2)))))

(define eqlist2?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
          ((or (null? l1) (null? l2)) #f)
          (else (and (equal? (car l1) (car l2)) (equal? (cdr l1) (cdr l2)))))))

(define rember'
  (lambda (s l)
    (cond ((null? l) '())
          ((equal? (car l) s) (cdr l))
          (else (cons (car l) (rember' s (cdr l)))))))

(define numbered?
  (lambda (aexp)
    (cond ((atom? aexp) (number? aexp))
          ((eq? (car (cdr aexp)) (quote +)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
          ((eq? (car (cdr aexp)) (quote x)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
          ((eq? (car (cdr aexp)) (quote ^)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

(define x
  (lambda (m n)
    (cond ((zero? m) 0)
          (else (o+ n (x n (sub1 m)))))))

(define ^
  (lambda (n m)
    (cond ((zero? m) 1)
          (else (x n (^ n (sub1 m)))))))

(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          ((eq? (car (cdr nexp) (quote +))) (o+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
          ((eq? (car (cdr nexp) (quote x))) (x (value (car nexp)) (value (car (cdr (cdr nexp))))))
          ((eq? (car (cdr nexp) (quote ^))) (^ (value (car nexp)) (value (car (cdr (cdr nexp)))))))))

(define value2
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          ((eq? (car nexp) (quote +)) (o+ (value2 (car (cdr nexp))) (value2 (car (cdr (cdr nexp))))))
          ((eq? (car nexp) (quote x)) (x (value2 (car (cdr nexp))) (value2 (car (cdr (cdr nexp))))))
          ((eq? (car nexp) (quote ^)) (^ (value2 (car (cdr nexp))) (value2 (car (cdr (cdr nexp)))))))))
          
(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr (aexp))))))
          
(define operator
  (lambda (aexp)
    (car aexp)))

(define value3
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          ((eq? (operator nexp) (quote +)) (o+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
          ((eq? (operator nexp) (quote x)) (x (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
          ((eq? (operator nexp) (quote ^)) (^ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))))))

(define sero?
  (lambda (n)
    (null? n)))
(define edd1
  (lambda (n)
    (cons '() n)))
(define zub1
  (lambda (n)
    (cdr n)))

(zub1 '(()()))

(define o+2
  (lambda (n m)
    (cond ((sero? m) n)
          (else (edd1 (o+2 n (zub1 m)))))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (equal? (car lat) a) (member? a (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((eq? (car lat) a) (multirember a (cdr lat)))
          (else (cons (car lat) (multirember a (cdr lat)))))))


(define set?
  (lambda (lat)
    (cond ((null? lat) #t) 
          ((member? (car lat) (cdr lat)) #f)
          (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond ((null? lat) '())
          ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
          (else (cons (car lat) (makeset (cdr lat)))))))

(define makeset2
  (lambda (lat)
    (cond ((null? lat) '())
          (else (cons (car lat) (makeset2 (multirember (car lat) (cdr lat))))))))

               
(makeset2 '(apple 3 pear 4 9 apple 3 4))
(makeset '(apple peach pear peach plum apple lemon peach))
(makeset2 '(apple peach pear peach plum apple lemon peach))

(define subset?
  (lambda (set1 set2)
    (cond ((null? set1) #t)
          (else (and (member? (car set1) set2) (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond ((or (null? set1) (null? set2)) #f)
          (else (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond  ((null? set1) '())
           ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
           (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
          ((member? (car set1) set2) (union (cdr set1) set2))
          (else (cons (car set1) (union (cdr set1) set2))))))

(define difference
  (lambda (set1 set2)
    (cond ((null? set1) '())
          ((member? (car set1) set2) (difference (cdr set1) set2))
          (else (cons (car set1) (difference (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond ((null? (cdr l-set)) (car l-set))
          (else (intersect (car l-set) (intersectall (cdr l-set)))))))

(intersectall '('(6 pears and) '(3 peaches and 6 peppers) '(8 pears and 6 plums) '(and 6 prunes with some apples)))

(define a-pair?
  (lambda (x)
    (cond ((atom? x) #f)
          ((null? x) #f)
          ((null? (cdr x)) #f)
          ((null? (cdr (cdr x))) #t)
          (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define third
  (lambda (p)
    (car (cdr (cdr p)))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define firsts
  (lambda (l)
    (cond ((null? l) '())
          (else (cons (car (car l)) (firsts (cdr l)))))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel
  (lambda (rel)
    (cond ((null? rel) (quote ()))
          (else (cons (revpair (first rel)) (revrel (cdr rel)))))))

(revrel '('(8 a) '(pumpkin pie) '(got sick)))

(define fullfun?
  (lambda (fun)
    (fun? (revrel fun))))

(define seconds
  (lambda (l)
    (cond ((null? l) '())
          (else (cons (second (car l)) (seconds (cdr l)))))))

(define fullfun2?
  (lambda (fun)
    (set? (seconds fun))))

(fullfun? '('(2 3) '(4 5) '(9 3)))
(fullfun? '('(2 3) '(4 5) '(9 7)))

(define rember-f
  (lambda (test? a l)
    (cond ((null? l) '())
          ((test? a (car l)) (cdr l))
          (else (cons (car l) (rember-f test? a (cdr l)))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? a x))))

(define eq?-salad
  (lambda (a)
    ((eq?-c 'salad) a)))

(define rember-f2
  (lambda (test?)
          (lambda (a l)
            (cond ((null? l) '())
                  ((test? a (car l)) (cdr l))
                  (else (cons (car l) ((rember-f2 test?) a (cdr l))))))))


(define rember-eq?
  (lambda (a l)
    ((rember-f2 eq?) a l)))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond ((null? l) '())
            ((test? (car l) old) (cons new (cons old (cdr l))))
            (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond ((null? l) '())
            ((test? (car l) old) (cons (car l) (cons new (cdr l))))
            (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))
   
(define seqL
  (lambda (new old l)
    (cons (new (cons old l)))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond ((null? l) '())
            ((eq? (car l) old) (seq new old l))
            (else (cons (car l) ((insert-g seq) new old (cdr l))))))))

(define insertL
  (lambda (new old l)
    ((insert-g seqL) new old l)))

(define insertR
  (lambda (new old l)
    ((insert-g seqR) new old l)))

(define insertL2
  (lambda (new old l)
    (insert-g (cons new (cons old l)))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst
  (insert-g seqS))

(define atom-to-function
  (lambda (x)
    (cond ((eq? x (quote +)) o+)
          ((eq? x (quote x)) x)
          (else ^))))

(define value4
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          (else ((atom-to-function (operator nexp)) (value4 1st-sub-exp nexp) (value4 2nd-sub-exp nexp))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond ((null? lat) '())
            ((test? (car lat) a) ((multirember-f test?) a (cdr lat)))
            (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

(define multirember-eq
  (multirember-f eq?))


(define eq?-tuna
  (eq?-c 'tuna))

(define multiremberT
  (lambda (test? lat)
    (cond ((null? lat) '())
          ((test? (car lat)) (multiremberT test? (cdr lat)))
          (else (cons (car lat) (multiremberT test? (cdr lat)))))))

(define a-friend
  (lambda (w y)
    (null? y)))

(define multirember&co
  (lambda (a lat col)
    (cond ((null? lat) (col '() '()))
          ((eq? (car lat) a) (multirember&co a (cdr lat) (lambda (newlat seen)
                                                     (col newlat (cons (car lat) seen)))))
          (else (multirember&co a (cdr lat) (lambda (newlat seen) (col (cons (car lat) newlat) seen)))))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond ((null? lat) '())
          ((eq? (car lat) oldL) (seqL new oldL (multiinsertLR new oldL oldR (cdr lat))))
          ((eq? (car lat) oldR) (seqR new oldR (multiinsertLR new oldL oldR (cdr lat))))
          (else (cons (car lat) (multiinsertLR (cdr lat)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond ((null? lat) (col '() 0 0))
          ((eq? (car lat) oldL) (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat nL nR)
                                                                             (col (cons new (cons oldL newlat)) (add1 nL) nR))))
          ((eq? (car lat) oldR) (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat nL nR)
                                                                            (col (cons oldR (cons new newlat)) nL (add1 nR)))))
          (else (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat nL nR)
                                                            (col (cons (car lat) newlat) nL nR)))))))


(define even?
  (lambda (n)
    (= (x (% n 2) 2) n)))


(define evens-only*
  (lambda (l)
    (cond ((null? l) '())
          ((atom? (car l)) (cond
                             ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
                             (else (evens-only* (cdr l)))))
          (else (cons (evens-only* (car l)) (evens-only* (cdr l)))))))

(define evens-only*&co
  (lambda (l col)
    (cond  ((null? l) (col '() 1 0))
           ((atom? (car l)) (cond
                              ((even? (car l)) (evens-only*&co (cdr l) (lambda (newl p s)
                                                                         (col (cons (car l) newl) (x (car l) p) s))))
                              (else (evens-only*&co (cdr l) (lambda (newl p s)
                                                              (col newl p (o+ (car l) s)))))))
           (else (evens-only*&co (car l) (lambda (al ap as)
                                                 (evens-only*&co (cdr l) (lambda (dl dp ds)
                                                                           (col (cons al dl) (x ap dp) (o+ as ds))))))))))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum (cons product newl))))

(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)

(define pick
  (lambda (n lat)
    (cond ((zero? (sub1 n)) (car lat))
          (else (pick (sub1 n) (cdr lat))))))

(define keep-looking
  (lambda (a sorn lat)
    (cond ((number? sorn) (keep-looking a (pick sorn lat) lat))
          (else (eq? sorn a)))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define shift
  (lambda (pair)
    (build (first (first pair)) (build (second (first pair)) (second pair)))))

(define length*
  (lambda (pora)
    (cond ((atom? pora) 1)
          (else (o+ (length* (first pora)) (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond ((atom? pora) 1)
          (else (o+ (x (weight* (first pora))) (weight* (second pora)))))))

(define shuffle
  (lambda (pora)
    (cond ((atom? pora) pora)
          ((a-pair? (first pora)) (shuffle (revpair pora)))
          (else (build (first pora) (shuffle (second pora)))))))
             
(define eternity
  (lambda (x)
    (eternity x)))

(define XXX
  (lambda (l)
    (cond ((null? l) 0)
          (else (add1 (lambda (l)
                        (cond ((null? l) 0)
                              (else (add1 (eternity (cdr l)))))) (cdr l))))))

(define new-entry
  build)

(build '(beverage dessert) '((food is) (number one with us)))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name (first entry) (second entry) entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond ((null? names) (entry-f name))
          ((eq? (car names) name) (car values))
          (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))
; by having enty-f as a variable, we leave for the user the possibility of determining what happens when the specified name is not in the entry

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond ((null? table) (table-f name))
          (else (lookup-in-entry name (car table) (lambda (name)
                                                    (lookup-in-table name (cdr table) table-f)))))))

(define expression-to-action
  (lambda (e)
    (cond((atom? e) (atom-to-action e))
         (else (list-to-action e)))))

(define atom-to-action
  (lambda (a)
    (cond ((number? a) *const)
          ((eq? a #f) *const)
          ((eq? a #t) *const)
          ((eq? a (quote cons)) *const)
          ((eq? a (quote car)) *const)
          ((eq? a (quote cdr)) *const)
          ((eq? a (quote null?)) *const)
          ((eq? a (quote eq?)) *const)
          ((eq? a (quote atom?)) *const)
          ((eq? a (quote zero?)) *const)
          ((eq? a (quote add1)) *const)
          ((eq? a (quote sub1)) *const)
          ((eq? a (quote number?)) *const)
          (else *identifier))))

(define list-to-action
  (lambda (l)
    (cond ((atom? (car l))
           (cond ((eq? (car l) (quote quote)) *quote)
                 ((eq? (car l) (quote lambda)) *lambda)
                 ((eq? (car l) (quote cond)) *cond)
                 (else *application)))
          (else *application))))

(define value10
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))
                        
(define *const
  (lambda (e table)
    (cond ((number? e) e)
          ((eq? e #t) #t)
          ((eq? e #f) #f)
          (else (build (quote primitive) e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of
  second)
  
(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car '())))

(define *lambda
  (lambda (e table)
   (build (quote non-primitive) (cons table (cdr e)))))

(define table-of first)
(define formals-of second)
(define body-of third)

(define evcon
  (lambda (lines table)
    (cond ((else? (question-of (car lines))) (meaning (answer-of (car lines)) table))
          ((meaning (question-of (car lines)) table) (meaning (answer-of (car lines)) table))
          (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    ((atom? x) (eq? (quote else) x))
    (else #f)))

(define question-of first)
(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evlis
  (lambda (args table)
    (cond ((null? args) '())
          (else (cons (meaning (car args)) (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (apply (meaning (function-of e) table) (evlis (arguments-of e) table))))

(define arguments-of
    (cdr e))

(define function-of
    (car e))

(define apply
  (lambda (fun vals)
    (cond ((primitive? fun) (apply-primitive (second fun) vals))
          ((non-primitive? fun) (apply-closure (second fun) vals)))))
    
(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))
    
(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))
    
(define apply-primitive
  (lambda (name vals)
    (cond ((eq? name (quote cons)) (cons (first vals) (second vals)))
          ((eq? name (quote car)) (car (first vals)))
          ((eq? name (quote cdr)) (cdr (first vals))) ;a function can have more than one argument, but car and cdr take only one- so why write (first vals)?
          ((eq? name (quote null?)) (null? (first vals)))
          ((eq? name (quote eq?)) (eq? (first vals) (second vals)))
          ((eq? name (quote atom?)) (:atom? (first vals)))
          ((eq? name (quote zero?)) (zero? (first vals)))
          ((eq? name (quote add1)) (add1 (first vals)))
          ((eq? name (quote sub1)) (sub1 (first vals)))
          ((eq? name (quote number?)) (number? (first vals))) 

(define :atom?
  (lambda (x)
    (cond ((atom? x) #t)
          ((null? x) #f)
          ((eq? (car x) (quote primitive)) #t)
          ((eq? (car x) (quote non-primitive)) #t) ;why? are functions atoms?
          (else #f))))

; applying a non-primitive function to a list of values is the same as finding out the meaning of the body of that function with its table extended by an entry with formals of the function and evlist (list of arguments)
(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure) (estend-table (new-entry (formals-of closure) vals) (table-of closure)))))

