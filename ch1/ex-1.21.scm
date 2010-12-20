;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 1 Section 2 Procedures and the Processes They Generate

;;; Exercise 1.21

; Use the smallest-divisor procedure to find the smallest divisor of each of the following numbers: 199, 1999, 19999.
;
;1 ]=> (smallest-divisor 199)
;Value: 199
;1 ]=> (smallest-divisor 1999)
;Value: 1999
;1 ]=> (smallest-divisor 19999)
;Value: 7
;1 ]=> 

; This is sort of a non-exercise but for completeness reproduce these function here:

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

