;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 1 Section 2 Procedures and the Processes They Generate

;;; Exercise 1.12 Pascal's triangle

;;     1
;;    1 1
;;   1 2 1
;;  1 3 3 1
;; 1 4 6 4 1


;; Here is one solution, or partial solution.  The question does not specify
;; exactly what it means to compute the elements, for instance should the 
;; function accept a single value and return that level of the triangle, or
;; compute the entire triangle up to that level?
;; Below is a simpler solution which accepts 2 values as input, the x and y
;; coordinates of a single element in the triangle. The inputs are 
;; considered to be counting from 0, ie the first element in the triangle is 
;; (0,0).

(define pascal
  (lambda (x y)
    (cond ((and (= y 0) (= x 0)) 1)
          ((= x 0) 1)
          ((= y 0) 0)
          (else
            (+ (pascal x (- y 1))
               (pascal (- x 1) (- y 1)))))))
