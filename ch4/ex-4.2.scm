;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 4 Section 1 The Metacircular Evaluator

;;;; Exercise 4.2

;; a. The problem with Louis's plan is that the predicate for determining if
;; an expression is an application simply checks if it is a pair, which would
;; result in any expression like "(define x 3)" to be taken as an application.

;; b.

(define (application? exp)
  (and (pair? exp)
       (equal? (car exp) 'call)))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
