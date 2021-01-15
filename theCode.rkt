#lang racket
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rember a
                          (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote()))
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((eq? (car lat) old)
          (cons old
                (cons new (cdr lat))))
         (else
          (cons (car lat)
                (insertR new old (cdr lat)))))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))      
      ((eq? (car lat) old) (cons new lat))
      (else
       (cons (car lat) (insertL new old (cdr lat)))))))


(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((eq? (car lat) old) (cons new (cdr lat)))
         (else
          (cons (car lat) (subst new old (cdr lat)))))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond 
      ((null? lat) (quote()))
      ((or (eq? (car lat) o1) 
           (eq? (car lat) o2)) (cons new (cdr lat)))
      (else
       (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((eq? (car lat) a)
          (multirember a (cdr lat)))
         (else (cons (car lat)
                (multirember a
                             (cdr lat)))))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((eq? (car lat) old)
          (cons old
                (cons new (multiinsertR new old
                                        (cdr lat)))))
         (else
          (cons (car lat)
                (multiinsertR new old
                              (cdr lat)))))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old)
       (cons new
             (cons old
                   (multiinsertL new old (cdr lat)))))
      (else
       (cons (car lat)
             (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))          
      ((eq? (car lat) old)
       (cons new
             (multisubst new
                         old
                         (cdr lat))))
      (else (cons (car lat)
                  (multisubst new
                              old
                              (cdr lat)))))))

(define myplus
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else
       (myplus (add1 a) (sub1 b))))))

(define mysubtract
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else
       (mysubtract (sub1 a) (sub1 b))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else
       (+ (car tup) (addtup (cdr tup)))))))

(define mytimes
  (lambda (m n)
    (cond
      ((zero? n) 0)
      (else
       (+ m (mytimes m (sub1 n)))))))
       
     
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons (+ (car tup1) (car tup2))
             (tup+ (cdr tup1) (cdr tup2)))))))
               
(define gt
  (lambda (a b)
    (cond
      ((zero? a) #f)
      ((zero? b) #t)      
      (else
       (gt (sub1 a) (sub1 b))))))

(define lt
  (lambda (a b)
    (cond
      ((zero? b) #f)
      ((zero? a) #t)          
      (else
       (lt (sub1 a) (sub1 b))))))

(define =*
  (lambda (a b)
    (cond      
      ((< a b) #f)
      ((> a b) #f)
      (else #t))))

(define tothe
  (lambda (a b)
    (cond
      ((zero? b) 1)
      (else
       (* a (tothe a (sub1 b)))))))

(define foo
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (foo (- n m) m))))))

(define mylength
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (mylength (cdr lat)))))))
       
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((member? (pick n lat) lat)
         (rember (pick n lat) lat))
         (else lat))))))
       
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((number? (car lat))
          (no-nums (cdr lat)))
         (else
          (cons (car lat) (no-nums (cdr lat)))))))))


(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((not (number? (car lat)))
          (all-nums (cdr lat)))
         (else
          (cons (car lat) (all-nums (cdr lat)))))))))

       
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2))
       (= a1 a2))
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else
       (cond
         ((eqan? (car lat) a)
          (add1 (occur a (cdr lat))))
         (else
          (occur a (cdr lat))))))))

(define one?
  (lambda (n)
    (cond
      (= n 1))))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      (else
       (cond
         ((atom? (car l))
          (cond
            ((eq? (car l) a)
             (rember* a (cdr l)))
            (else
             (cons (car l) (rember* a (cdr l))))))
         (else (cons (rember* a (car l)) (rember* a (cdr l)))))))))
          
          
(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())      
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons old
                (cons new
                      (insertR* new old (cdr l)))))
         (else
          (cons (car l)
                (insertR* new old
                          (cdr l))))))
      (else
       (cons (insertR* new old
                       (car l))
             (insertR* new old
                       (cdr l)))))))
           
               
          
(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)            
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l))
               (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (subst* new old (cdr l))))
         (else (cons (car l)
                     (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l))
                  (subst* new old (cdr l)))))))

(define insertL*  
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond 
         ((eq? (car l) old)
          (cons new (cons old
                          (insertL* new old (cdr l)))))
         (else (cons (car l)
                     (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l))
                  (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? (car l) a)
           (member* a (cdr l))))              
      (else (or (member* a (car l))
                (member* a (cdr l)))))))
                  
                   
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))
       
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1))
            (atom? (car l2)))
       (and (eqan? (car l1)
                    (car l2))
             (eqlist? (cdr l1)
                      (cdr l2))))
      ((or (atom? (car l1)) 
           (atom? (car l2))) #f)         
      (else (and (eqlist? (car l1)
                          (car l2))
                 (eqlist? (cdr l1)
                          (cdr l2)))))))
(define myEqual?
  (lambda (l1 l2)
    (cond
      ((and (atom? l1) (atom? l2))
       (eqan? l1 l2))
      ((or (atom? l1) (atom? l2)) #f)
      (else (eqlist? l1 l2)))))

             
             
           

      

 
(myEqual? '((beef (foo) (foo)))
          '((beef (foo) (foo))))