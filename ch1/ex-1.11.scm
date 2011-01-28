;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 1 Section 2 Procedures and the Processes They Generate

;;; Exercise 1.11

(define f-rec
  (lambda (n)
    (if (< n 3)
        n
        (+ (f-rec (- n 1))
           (* 2 (f-rec (- n 2)))
           (* 3 (f-rec (- n 3)))))))

(define f-iter-start
  (lambda (n)
    (if (< n 3)
        n
        (f-iter n 3 0 1 2))))

(define f-iter
  (lambda (n i a b c)
    (if (= n i)
        (+ c (+ b b) (+ a a a))
        (f-iter n (+ i 1) b c (+ c (+ b b) (+ a a a))))))
