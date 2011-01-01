;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 4 Section 3 Variations on a Scheme - Nondeterministic
;;;; Computing

;;;; Exercise 4.35

(define (an-integer-between low high)
  (require (not (> low high)))
  (amb i (an-integer-between (+ 1 low) high)))
