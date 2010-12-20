;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 1 Section 2 Procedures and the Processes They Generate

;;; Exercise 1.22

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
       (report-prime (- (runtime) start-time) n)
      #f))

(define (report-prime elapsed-time n)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time)
  #t)

(define search-for-primes
  (lambda (x)
    (search-for-primes-start x 3)))

(define search-for-primes-start
  (lambda (x n)
    (cond ((= n 0) '() )
          ((timed-prime-test x) (search-for-primes-start (+ x 1) (- n 1)))
          (else (search-for-primes-start (+ x 1) n)))))
                
; dependencies from previous in the primality test discussion...
(define (prime? n) (= n (smallest-divisor n)))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

; RESULTS:
;  1 ]=> (search-for-primes 10000)
;  
;  10007 *** 0.
;  10009 *** 0.
;  10037 *** 0.
;  ;Value: ()
;  
;  1 ]=> (search-for-primes 100000)
;  
;  100003 *** 0.
;  100019 *** 0.
;  100043 *** 0.
;  ;Value: ()
;  
;  1 ]=> (search-for-primes 1000000)
;  
;  1000003 *** 1.0000000000000009e-2
;  1000033 *** .01999999999999999
;  1000037 *** 0.
;  ;Value: ()
;  
;  1 ]=> (search-for-primes 10000000)
;  
;  10000019 *** .01999999999999999
;  10000079 *** 2.0000000000000018e-2
;  10000103 *** 2.0000000000000018e-2
;  ;Value: ()
;  
;  1 ]=> (search-for-primes 100000000)
;  
;  100000007 *** .09000000000000002
;  100000037 *** .08999999999999997
;  100000039 *** .07999999999999996
;  ;Value: ()
;  
;  1 ]=> (search-for-primes 1 000 000 000)
;  
;  1000000007 *** .28
;  1000000009 *** .2899999999999999
;  1000000021 *** .26
;  ;Value: ()
;  
;  1 ]=> (search-for-primes 10 000 000 000)
;  
;  10000000019 *** .8200000000000003
;  10000000033 *** .8099999999999996
;  10000000061 *** .8199999999999998
;  ;Value: ()
;  
;  1 ]=> (search-for-primes 100 000 000 000)
;  
;  100000000003 *** 2.6399999999999997
;  100000000019 *** 2.67
;  100000000057 *** 2.620000000000001
;  ;Value: ()
;  
