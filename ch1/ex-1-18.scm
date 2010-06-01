;  Using the results of exercises 1.16 and 1.17, devise a procedure that generates an iterative process for multiplying two integers in terms of adding, doubling, and halving and uses a logarithmic number of steps.

(define fast-mult-iter
  (lambda (x y z)
    (cond ((= y 0) z)
          ((even? y) (fast-mult-iter (double x) (halve y) z))
          (else (fast-mult-iter x (- y 1) (+ z x))))))
