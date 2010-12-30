;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 4 Section 1 The Metacircular Evaluator

;;;; Exercise 4.21 

;; factorial using the y-combinator
((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)

;; a. Fibonacci numbers:

((lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (fb k)
      (if (= k 0)
          0
          (if (= k 1)
              1
              (+ (fb fb (- k 1)) (fb fb (- k 2))))))))
 10)

;; b. mutually recursive procedures

(define (f x)
  (define (even? n)
    (if (= n 0)
        true
        (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0)
        false
        (even? (- n 1))))
  (even? x))

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))
