;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 1 Section 3 Formulating Abstractions with Higher-Order Procedures

;;; Exercise 1.32

; Recursive accumulate function
(define accumulate
  (lambda (combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (accumulate combiner null-value term (next a) next b)))))

; 2 error> (accumulate + 0 (lambda (x) x) 1 (lambda (x) (+ x 1)) 10)
;Value: 55
; 2 error> (accumulate * 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) 5)
;Value: 120

; ... and iterative one:
(define accumulate
  (lambda (combiner null-value term a next b)
    (define iter
      (lambda (a result)
        (if (> a b)
            result
            (iter (next a) (combiner result (term a))))))
    (iter a null-value))) ; null-value goes here at the start
