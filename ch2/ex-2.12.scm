;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 2 Section 1 Introduction to Data Abstraction

;;; Exercise 2.12

; Exercise 2.12
(define make-center-percent
  (lambda (c p)
    (let ((w (/ (* c p) 100.0)))
      (make-interval (- c w)
                     (+ c w)))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define percent
  (lambda (i)
    (* 100 (/ (width i) (center i)))))
