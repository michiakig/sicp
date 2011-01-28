;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 1 Section 2 Procedures and the Processes They Generate

;;; Exercise 1.26

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         ;; The problem is that here the function is called recursively twice
         (remainder (* (expmod base (/ exp 2) m)
                       ;; instead of only a single time and returning
                       ;; a result which is squared
                       (expmod base (/ exp 2) m))   
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))


