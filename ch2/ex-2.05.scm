;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 2 Section 1 Introduction to Data Abstraction

;;; Exercise 2.05 pairs of non-negative integers represented as (2^a)(3^b)

(define cons
  (lambda (a b)
    (* (expt 2 a) ; doesn't check for sign
       (expt 3 b))))

(define car
  (lambda (c)
    (cond ((= 0 (remainder c 2)) (+ 1 (car (/ c 2))))
          (else 0))))

(define cdr
  (lambda (c)
    (cond ((= 0 (remainder c 3)) (+ 1 (cdr (/ c 3))))
          (else 0))))
