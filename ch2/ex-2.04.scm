;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 2 Section 1 Introduction to Data Abstraction

;;; Exercise 2.04 cons, car, and cdr as closures

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

;; This idea is similarly presented in "The Seasoned Schemer" (maybe Felleisen
;; and Friedman borrow it from SICP?).  Even having seen it before, it's still
;; mind-blowing.  Appropriate definition of cdr...

(define (cdr z)
  (z (lambda (p q) q)))
