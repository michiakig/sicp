;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 2 Section 2 Hierarchical Data and the Closure Property

;;; Exercise 2.17

(define last-pair
  (lambda (items)
    (cond ((null? (cdr items)) items)
          (else (last-pair (cdr items))))))
