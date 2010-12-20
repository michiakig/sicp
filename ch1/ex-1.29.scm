; Example function to approximate the integral of
(define (cube x) (* x x x))

; This is the "vanilla" sum function from 1.3
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

; Used to pick the factor of 2 or 4 for each term in sum
(define (even? x) (= (remainder x 2) 0))

; The implementation of Simpson's rule, including term and next functions:
;
; Calculating the kth term depends on a, h (therefore b), and k. By defining
; "next" and "term" inside the function we need only k to find the kth term
; in the sum. Instead of calling sum with a and b, call it with 1 and n-1,
; which are the start and end values of k.
;
; 1 ]=> (simpson cube 0 1.0 100)
;Value: .24999999999999992
; 1 ]=> (simpson cube 0 1.0 1000)
;Value: .2500000000000003


(define simpson
  (lambda (f a b n)
      (let ((h (/ (- b a) n)))
        (define term (lambda (k) (* (if (even? k) 2 4) (f (+ a (* k h))))))
        (define next (lambda (x) (+ x 1)))
        (* (/ h 3)
           (+ (f a)
              (sum term 1 next (- n 1))
              (f (+ a (* n h)))))
        )))
; This is interesting, I think it demonstrates another flexibility of
; first-class functions: there is no need to change "sum," by defining the
; "next" and "term" functions in terms of the index k instead of the x value 
; Maybe that's the point of the exercise...
;
; I cheated by using "let" since it isn't introduced until the next section,
; but that's the result of reading the entire chapter before the exercises
;
