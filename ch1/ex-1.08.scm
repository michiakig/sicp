;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 1 Section 1 Elements of Programming

;;; Exercise 1.08

; SICP exercise 1.8
; Newton's method for cube roots is based on the fact that if y is an approximation to the cube root of x, then a better approximation is given by the value
; (x/(y^2)+2y)/3
; Use this formula to implement a cube-root procedure analogous to the square-root procedure.

(define cube-root (lambda (x) (cube-root-iter x 1.0)))

(define cube-root-iter
	(lambda (x guess)
		(if (good-enough? x guess)
				guess
				(cube-root-iter x (improve x guess)))))

(define good-enough?
	(lambda (x guess)
		(< (abs (- x (* guess guess guess))) 0.00001)))	

(define improve
	(lambda (x y)
		(/	(+	(/	x
								(* y y))
						(+ y y))
				3.0)))
