;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 3 Section 1 Assignment and Local State

;;;; Exercise 3.1

;;; This is strangely familiar...
;;; http://www.paulgraham.com/icad.html

(define (make-accumulator n)
  (lambda (i)
    (set! n (+ n i))
    n))
