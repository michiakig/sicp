;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 5 Section 3 Storage Allocation and Garbage Collection

;;; Exercise 5.20

;; this is a bit crude
index     0  1  2  3  4  5
the-cars    n1 p1 p1
the-cdrs    n2 e0 p2

;; the free pointer will be p3
;; x will be p1, y will be p3

;;; Exercise 5.21

;; a. Recursive count-leaves:



(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

;; b. Recursive count-leaves with explicit counter:

(define (count-leaves tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else (count-iter (cdr tree)
                            (count-iter (car tree) n)))))
  (count-iter tree 0))

