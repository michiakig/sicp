;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 1 Section 1 Elements of Programming

;;; Exercise 1.08 Newton's method for cube roots

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
    (/ (+ (/ x
             (* y y))
          (+ y y))
       3.0)))
