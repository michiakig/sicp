;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 1 Section 3 Formulating Abstractions with Higher-Order Procedures

;;; Exercise 1.42

(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

(define repeated
  (lambda (f n)
    (if (= n 1)
        f
        (compose f (repeated f (- n 1))))))

(define smooth
  (lambda (f)
    (lambda (x)
      (/ (+ (f (- x dx))
            (f x)
            (f (+ x dx)))
         3))))

(define n-fold-smooth
  (lambda (f n)
    (lambda (x)
      (((repeated smooth n) f) x)
