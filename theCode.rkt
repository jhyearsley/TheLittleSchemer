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
      (else (cons (first (car l))
                  (firsts (cdr l)))))))

(define seconds
  (lambda (l)
    (cond
      ((null? l) (quote()))
      (else (cons (second (car l))
                  (seconds (cdr l)))))))

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

(define integerdivision
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (integerdivision (- n m) m))))))

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

             
             
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))       
      (else
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp)))))))))

(define value1
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) '+)
       (+ (value1 (car nexp))
          (value1 (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) '*)
       (* (value1 (car nexp))
          (value1 (car (cdr (cdr nexp))))))
      (else
       (tothe (value1 (car nexp))
              (value1 (car (cdr (cdr nexp)))))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))
       
(define value2
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+)
       (+ (value2 (1st-sub-exp nexp))
          (value2 (2nd-sub-exp nexp))))
      ((eq? (operator nexp) '*)
       (* (value2 (1st-sub-exp nexp))
          (value2 (2nd-sub-exp nexp))))
      (else
       (tothe (value2 (1st-sub-exp nexp))
              (value2 (2nd-sub-exp nexp)))))))


(define isZero?
  (lambda (n)
    (null? n)))

(define likeAdd1
  (lambda (n)
    (cons '() n)))

(define likeSub1
  (lambda (n)
    (cdr n)))

(define myPlus
  (lambda (m n)
    (cond
      ((isZero? n) m)
      (else (myPlus (likeAdd1 m) (likeSub1 n))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())      
      (else
       (cons (car lat)
             (makeset
              (rember* (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((and (member? (car set1) set2)
            (subset? (cdr set1) set2)))
      (else #f))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))
      
(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else
       (or (member? (car set1) set2)
           (intersect? (cdr set1) set2))))))
       
       
(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (cons (car set1)
             (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else (cons (car set1)
                  (union (cdr set1) set2))))))

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (difference (cdr set1) set2))
      (else (cons (car set1)
                  (difference (cdr set1) set2))))))


    
                     
(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set)
                       (intersectall (cdr l-set)))))))
       
       
;(intersectall '((1 2 3) (1 2) (1)))
;(intersect '(1 2 3) (intersectall '((1 2) (1)))))
;(intersect '(1 2 3) (intersect '(1 2) (intersectall '((1)))))
;(intersect '(1 2 3) (intersect '(1 2) '(1)))
;(intersect '(1 2 3) '(1))
;'(1)

(define a-pair?
  (lambda (x)    
    (cond
      ((atom? x) #f)
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
  (lambda (l)
    (car (cdr (cdr l)))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else
       (cons (revpair (car rel))
             (revrel (cdr rel)))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote()))
        ((test? (car l) a) (cdr l))
        (else (cons (car l)
                    ((rember-f test?)
                              a
                              (cdr l))))))))
(define rember-eq?
  (rember-f eq?))

  
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)      
      (cond
        ((null? l) (quote()))      
        ((test? (car l) old)
         (cons new l))
        (else
         (cons (car l)
               ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote()))                 
        ((test? (car l) old)
         (cons old (cons new (cdr l))))
        (else
         (cons (car l)
               ((insertR-f test?) new old (cdr l))))))))
                             
(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))


(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) (quote()))      
        ((eq? (car l) old)
         (seq new old (cdr l)))
        (else
         (cons (car l)
               ((insert-g seq) new old (cdr l))))))))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x '+) +)
      ((eq? x '*) *)
      (else tothe))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
       ((atom-to-function (operator nexp))      
        (value (1st-sub-exp nexp))
        (value (2nd-sub-exp nexp)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) (quote()))               
        ((test? (car lat) a)
         ((multirember-f test?) a
                                (cdr lat)))
        (else (cons (car lat)
                    ((multirember-f test?) a
                                           (cdr lat))))))))

(define multirember-eq?
  (multirember-f eq?))

;(define even?
;  (lambda (n)
;    (= (* (/ n 2) 2) n)))

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) '())
      (else
       (cond
         ((atom? (car l))
          (cond
            ((even? (car l))
             (cons (car l)
                   (evens-only* (cdr l))))
            (else
             (evens-only* (cdr l)))))
         (else (cons (evens-only* (car l))
                     (evens-only* (cdr l)))))))))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))
                  
                  
(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (align (shift pora)))
      (else (build (first pora)
                   (align (second pora)))))))


(define new-entry build)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car values))
      (else
       (lookup-in-entry-help name
                             (cdr names)
                             (cdr values)
                             entry-f)))))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name
                             (car table)
                             (lambda (name)
                               (lookup-in-table name
                                                (cdr table)
                                                table-f)))))))
    
    
    


                         
        
