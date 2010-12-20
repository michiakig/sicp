;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 1 Section 2 Procedures and the Processes They Generate

;;; Exercise 1.27


(define carmichael-test
  (lambda (n)
    (carmichael-test-r 1 n)))

(define carmichael-test-r
  (lambda (a n)
    (cond ((= a n) #t)
          ((= (expmod a n n) a) (carmichael-test-r (+ a 1) n))
          (else #f))))

