;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 1 Section 1 Elements of Programming

;;; Exercise 1.01

; (not a programming exercise, but for completeness,
; and to make it easy to find unfinished exercises ...)

; Below is a sequence of expressions. What is the result printed by the interpreter in response to each expression? Assume that the sequence is to be evaluated in the order in which it is presented.

10
; 10
(+ 5 3 4)
; 12
(- 9 1)
; 8
(/ 6 2)
; 3
(+ (* 2 4) (- 4 6))
; 6
(define a 3)
; implementation defined?
; a
(define b (+ a 1))
; same
; b
(+ a b (* a b))
; 19
(= a b)
; #f
(if (and (> b a) (< b (* a b)))
    b
    a)
; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
; 16
(+ 2 (if (> b a) b a))
; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
; 16
