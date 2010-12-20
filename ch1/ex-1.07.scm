;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 1 Section 1 Elements of Programming

;;; Exercise 1.07

; SICP exercise 1.7
;  The good-enough? test used in computing square roots will not be very effective for finding the square roots of very small numbers. Also, in real computers, arithmetic operations are almost always performed with limited precision. This makes our test inadequate for very large numbers. Explain these statements, with examples showing how the test fails for small and large numbers. An alternative strategy for implementing good-enough? is to watch how guess changes from one iteration to the next and to stop when the change is a very small fraction of the guess. Design a square-root procedure that uses this kind of end test. Does this work better for small and large numbers?

; should probably use (let ... ) here but I'll pretend I don't know about it yet
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (sqrt_ x) (sqrt-iter 1.0 x))
(define new-good-enough?
	(lambda (old-guess new-guess)
		(< (abs (- old-guess new-guess)) (/ old-guess 1000000))))

(define (improve guess x) (average guess (/ x guess)))
(define (average x y) (/ (+ x y) 2))
(define (good-enough? guess x) (< (abs (- (square guess) x)) 0.001))
