; This was even more difficult to test than the naive prime search...
; Searching for primes above 100 000 000 000 000 000 000 000 000 did not 
; take long enough to return > 0

(define (start-prime-test n start-time)
  (if (fast-prime? n 3)
       (report-prime (- (runtime) start-time) n)
      #f))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

