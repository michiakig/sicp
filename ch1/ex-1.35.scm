;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 1 Section 3 Formulating Abstractions with Higher-Order Procedures

;;; Exercise 1.35

;; the proof that phi is a fixed point of x -> 1 + 1/x isn't that
;; interesting, it's just algebraic grunt work

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
;; 1.6180327868852458
