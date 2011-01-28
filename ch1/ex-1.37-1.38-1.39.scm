;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 1 Section 1 Elements of Programming

;;; Exercise 1.37 basic continued fraction function for
(define cont-frac
  (lambda (n d k)
    (define cont-frac-r
      (lambda (i)
        (cond ((= i k) (/ (n i) (d i)))
              (else (/ (n i) (+ (d i) (cont-frac-r (+ i 1))))))))
    (cont-frac-r 1)))

;;; Exercise 1.38 approximation of e from Euler's De Fractionibus
;;; Continuis 
(define d
  (lambda (i)
    (cond ((= i 1) 1)
          ((= i 2) 2)
          ((= (remainder (- i 2) 3) 0) (+ (d (- i 1)) (d (- i 2)) (d (- i 3))))
          (else 1))))

;;; Exercise 1.39 approximation of tangent by J H Lambert, 1770
(define tan
  (lambda (x k)
    (/ x
       (- 1 (cont-frac (lambda (i) (* x x))
                       (lambda (i) (+ 1 (* 2 i)))
                       k)))))
