;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 5 Section 5 Compilation

;;; Exercise 5.35 what expression was compiled to produce ... ?

(define (f x)
  (+ x (g (+ x 2))))
