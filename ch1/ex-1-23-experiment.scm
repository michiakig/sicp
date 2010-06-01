(define (smallest-divisor n)
  (if (divides? n 2)
      2
      (find-divisor n 3)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 2)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define next
  (lambda (test-divisor)
    (if (= test-divisor 2)
        3
        (+ test-divisor 2))))

