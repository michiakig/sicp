;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 1 Section 1 Elements of Programming

;;; Exercise 1.05

; SICP exercise 1.5
; Ben Bitdiddle has invented a test to determine whether the interpreter he is
; faced with is using applicative-order evaluation or normal-order evaluation.
; He defines the following two procedures:

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

; This will result in an infinite loop in an interpreter which uses applicative-
; order evaluation (most implementations of Scheme?).  The distinction between
; normal-order and applicative-order really interests me, because I found the
; discussion surrounding the derivation of the applicative-order Y combinator 
; in Friedman and Felleisen's The Little Schemer incredibly fascinating, and I
; did not fully understand the difference between the two.  I will read on with
; curiousity... 
