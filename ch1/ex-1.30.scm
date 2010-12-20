;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 1 Section 3 Formulating Abstractions with Higher-Order Procedures

;;; Exercise 1.30

; Iterative sum procedure, pretty straightforward
(define (sum term a next b)
  (define (iter a result)
    (if (> a b) 
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

