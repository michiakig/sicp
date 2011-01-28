;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 1 Section 3 Formulating Abstractions with Higher-Order Procedures

;;; Exercise 1.34

(f f)
(f 2)
(2 2)

;; The interpreter will generate an error because evaluating this
;; combination will result in the expression (2 2) and since 2 is not
;; a procedure, this application fails.
