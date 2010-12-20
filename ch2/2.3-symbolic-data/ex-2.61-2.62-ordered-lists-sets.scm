;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 2 Section 3 Symbolic Data

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;;; Exercise 2.61

;;; Implement adjoin-set using the ordered representation.

(define (adjoin-set x s)
  (cond ((null? s) (list x))
        ((= x (car s)) s)
        ((< x (car s)) (cons x s))
        (else (cons (car s) (adjoin-set x (cdr s))))))

;;; Exercise 2.61

;;; Give a Theta(n) implementation of union-set for sets as ordered lists.

(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((= (car s1) (car s2))
         (cons (car s1) (union-set (cdr s1) (cdr s2))))
        ((< (car s1) (car s2))
         (cons (car s1) (union-set (cdr s1) s2)))
        ((< (car s2) (car s1))
         (cons (car s2) (union-set s1 (cdr s2))))))
