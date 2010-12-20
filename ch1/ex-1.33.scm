(define filtered-accumulate
  (lambda (combiner null-value term a next b filter)
    (if (> a b)
        null-value
        (combiner (if (filter a) ; if a matches the criteria, combine it
                      (term a)
                      null-value) ; otherwise use the null-value here... + 0 or * 1 for instance should have no effect
                  (filtered-accumulate combiner null-value term (next a) next b filter)))))

(define (prime? x) (fast-prime? x 4))
; (filtered-accumulate + 0 (lambda (x) (* x x)) 2 (lambda (x) (+ x 1)) 10 prime?)

; 
