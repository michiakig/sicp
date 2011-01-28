;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 1 Section 2 Procedures and the Processes They Generate

;;; Exercise 1.20

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define r remainder)

;; normal-order evaluation
(gcd 206 40)

(if (= 40 0)
    206
    (gcd 40 (r 206 40)))

(gcd 40 (r 206 40))

(if (= (r 206 40) 0)
      40
      (gcd (r 206 40) (r 40 (r 206 40))))

(if (= 6 0) ; one r operation performed
      40
      (gcd (r 206 40) (r 40 (r 206 40))))

(gcd (r 206 40) (r 40 (r 206 40)))

(if (= (r 40 (r 206 40)) 0)
    (r 206 40)
    (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))

(if (= 4 0) ; two more r operations
    (r 206 40)
    (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))

(gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))

(if (= 2 0) ; 4 r operations
    (r 40 (r 206 40))
    (gcd (r (r 206 40) (r 40 (r 206 40)))
         (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))

(gcd (r (r 206 40) (r 40 (r 206 40)))
     (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))

(if (= (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))) 0)
    (r (r 206 40) (r 40 (r 206 40)))
    (gcd (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
         (r (r (r 206 40) (r 40 (r 206 40)))
            (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))

(if (= 0 0) ; 7
    2 ; 4
    (gcd (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
         (r (r (r 206 40) (r 40 (r 206 40)))
            (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))

;; total number of remainder operations: 18

;; applicative-order evaluation:

(gcd 206 40)

(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))

(gcd 40 (remainder 206 40)) ;

(gcd 40 6)

(if (= 6 0)
    40
    (gcd 6 (remainder 40 6)))

(gcd 6 (remainder 40 6)) ;

(gcd 6 4)

(if (= 4 0)
    6
    (gcd 4 (remainder 6 4)))

(gcd 4 (remainder 6 4)) ; 

(gcd 4 2)

(if (= 2 0)
    4
    (gcd 2 (remainder 4 2)))

(gcd 2 (remainder 4 2)) ; 

(gcd 2 0)

(if (= 0 0)
    2
    (gcd 0 (remainder 2 0)))

2

;; total number of remainder operations: 4
