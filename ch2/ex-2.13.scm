;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 2 Section 1 Introduction to Data Abstraction

;;; Exercise 2.13

; Show that under the assumption of small percentage tolerances there is a simple formula for the approximate percentage tolerance of the product of two intervals in terms of the tolerances of the factors. You may simplify the problem by assuming that all numbers are positive.

; After some algebraic grunt work:
; Given two intervals defined by midpoints and factors such as
; (6.8, .10) for 6.8 +/- 10%
; (x, p) and (y, q)
; The actual product is:
; (xy+xypq), (xyp+xyq)/(xy+xypq)
; For very small percentages, pq is even smaller.
; So this can be approximated as
; xy, p+q
;
; The Scheme function for this is simple:

(define (product x y)
  (make-center-percent (* (center x) (center y))
                       (+ (percent x) (percent y))))
