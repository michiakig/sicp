;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 4 Section 1 The Metacircular Evaluator

;;; Exercise 4.15 An old friend.

Assume that we do have a procedure halts? which determines if a procedure halts on a given input.

Let:

(define (run-forever) (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

If (p p) halts, then (try p) will run forever. If (p p) runs forever, (try p) will halt.

Consider (try try).

If (try try) halts, then (try try) will run forever. If (try try) runs forever, then (try try) will halt.

This is a direct contradiction, and so halts? cannot exist.


