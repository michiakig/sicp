;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 1 Section 2 Procedures and the Processes They Generate

;;; Exercise 1.17

; "... suppose we include, together with addition, operations double, which doubles an integer, and halve, which divides an (even) integer by 2. Using these, design a multiplication procedure analogous to fast-expt that uses a logarithmic number of steps."

(define double
  (lambda (x)
    (* 2 x)))

(define halve
  (lambda (x)
    (/ x 2)))

(define fast-mult
  (lambda (x y)
    (cond ((= y 1) x)
          ((even? y) (double (fast-mult x (/ y 2))))
          (else (+ x (fast-mult x (- y 1)))))))
