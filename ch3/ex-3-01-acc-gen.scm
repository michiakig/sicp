;; Exercise 3.1

;; Obligatory... http://www.paulgraham.com/icad.html 
(define (make-accumulator n)
  (lambda (i)
    (set! n (+ n i))
    n))
