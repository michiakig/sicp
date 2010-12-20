;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 1 Section 3 Formulating Abstractions with Higher-Order Procedures

;;; Exercise 1.41

(define double
  (lambda (f)
    (lambda (x)
      (f (f x)))))

(define inc (lambda (x) (+ x 1)))
