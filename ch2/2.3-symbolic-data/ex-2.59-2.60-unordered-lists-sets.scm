;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 2 Section 3 Symbolic Data

; Section 2.3.3, Example: representing sets

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;;; Exercise 2.59
;;; Implement union-set for sets as unordered lists

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set? (car set1) set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))

;;; Exercise 2.60.

;;; Allow duplicate elements in a set, write element-of-set?,
;;; union-set, intersection-set.
(define dupe-element-of-set? element-of-set?)
(define dupe-union-set append)
(define (dupe-intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((dupe-element-of-set? (car set1) set2)
         (cons (car set1) (dupe-intersection-set (cdr set1) set2)))
        (else (dupe-intersection-set (cdr set1) set2))))
 
