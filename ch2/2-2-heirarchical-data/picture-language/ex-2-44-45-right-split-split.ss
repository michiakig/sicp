#lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

; Exercise 2.44.  Define the procedure up-split used by corner-split. It is similar to right-split, except that it switches the roles of below and beside.

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

; Exercise 2.45.  Right-split and up-split can be expressed as instances of a general splitting operation. Define a procedure split with the property that evaluating
; (define right-split (split beside below))
; (define up-split (split below beside))
; produces procedures right-split and up-split with the same behaviors as the ones already defined.

(define (split b1 b2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split b1 b2) painter (- n 1))))
          (b1 painter (b2 smaller smaller))))))

