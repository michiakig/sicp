
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor))))) ; new function

(define next
  (lambda (test-divisor)
    (if (= test-divisor 2)
        3
        (+ test-divisor 2))))

; The results are below for the same set of input as the previous exercise.
; The expectation that it would run twice as fast is not really confirmed,
; the ratio is closer to 8/5 than to 2.
; 
; I'm not entirely sure how to explain this, except that perhaps the added
; procedure call and comparison are enough to slow it down this much.

;  RESULTS
;  1 ]=> (search-for-primes 1 000 000 000)
;  
;  1000000007 *** .16999999999999993
;  1000000009 *** .16000000000000014
;  1000000021 *** .16999999999999993
;  ;Value: ()
;  
;  1 ]=> (search-for-primes 10 000 000 000)
;  
;  10000000019 *** .4900000000000002
;  10000000033 *** .5
;  10000000061 *** .5099999999999998
;  ;Value: ()
;  
;  1 ]=> (search-for-primes 100 000 000 000)
;  
;  100000000003 *** 1.6099999999999994
;  100000000019 *** 1.6099999999999994
;  100000000057 *** 1.6300000000000026
;  ;Value: ()

; I did a quick experiment, and I believe my guess above was correct.
; After removing the extra procedure call and comparison, the ratio is 
; much closer to 2:

;  1 ]=> (search-for-primes 100000000000)
;  
;  100000000003 *** 1.299999999999999
;  100000000019 *** 1.3100000000000005
;  100000000042 *** 1.3100000000000005
;  ;Value: ()
;  
;  1 ]=> (search-for-primes 10000000000)
;  
;  10000000004 *** .4299999999999997
;  10000000019 *** .41000000000000014
;  10000000033 *** .41000000000000014
;  ;Value: ()
;  
;  1 ]=> (search-for-primes 1000000000)
;  
;  1000000006 *** .15000000000000036
;  1000000007 *** .13000000000000078
;  1000000009 *** .129999999999999
;  ;Value: ()
  
(define (smallest-divisor n)
  (if (divides? n 2) ; only perform this check once
      2
      (find-divisor n 3)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 2))))) ; no extra function call

; One final experiment, I will replace the next function with one which makes 
; a comparison and regardless of the result, simply increments its argument
; by one. This should result in a running time twice the original improvement.

(define next
  (lambda (test-divisor)
    (if (= test-divisor 2)
        (+ test-divisor 1)
        (+ test-divisor 1))))

; And the results seem to confirm this:
;
;  1 ]=> (search-for-primes 1000000000)
;  
;  1000000006 *** .32000000000000206
;  1000000007 *** .3099999999999987
;  1000000009 *** .3000000000000007
;  ;Value: ()
;  
;  1 ]=> (search-for-primes 10000000000)
;  
;  10000000019 *** .990000000000002
;  10000000033 *** .9899999999999984
;  10000000058 *** 1.
;  ;Value: ()
;  
;  1 ]=> (search-for-primes 100000000000)
;  
;  100000000003 *** 3.16
;  100000000019 *** 3.169999999999998
;  100000000042 *** 3.16
;  ;Value: ()
