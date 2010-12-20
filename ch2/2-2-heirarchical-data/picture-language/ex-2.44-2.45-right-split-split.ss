;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 2 Section 2 Hierarchical Data and the Closure Property

#lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;;; Exercise 2.44.

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;;; Exercise 2.45.

(define (split b1 b2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split b1 b2) painter (- n 1))))
          (b1 painter (b2 smaller smaller))))))

