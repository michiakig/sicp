;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 2 Section 1 Introduction to Data Abstraction

;;; Exercise 2.11

; "... by testing sign of end-points, it is possible to break
; mul-interval up into 9 cases, only one of which requires more
; than 2 multiplications"
; orig. mul-interval below

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y))) 
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (neg? x) (< x 0))
(define (pos? x) (> x 0))
(define up upper-bound)
(define low lower-bound)

; this is a tedious solution, which makes me think there may be a smarter way 
; vim is actually slowed by having to handle the parens matching on this...
(define (mul-interval2 x y)
  (cond ((and (pos? (low x)) (pos? (up x)))(cond ((and (pos? (low y)) (pos? (up y)))
                                                   (display "here we are")
                                                   (make-interval (* (low x) (low y)) (* (up x) (up y))))
                                                  ((and (neg? (low y)) (neg? (up y)))
                                                   (make-interval (* (up x) (low y)) (* (low x) (up y))))
                                                  ((and (neg? (low y)) (pos? (up y)))
                                                   (make-interval (* (low y) (up x)) (* (up x) (up x))))
                                                  (else (error "bad interval"))))
        ((and (neg? (low x)) (pos? (up x))) (cond ((and (pos? (low y)) (pos? (up y)))
                                                   (make-interval (* (low x) (up y)) (* (up x) (up y))))
                                                  ((and (neg? (low y)) (neg? (up y)))
                                                   (make-interval (* (up x) (low y)) (* (low x) (low y))))
                                                  ((and (neg? (low y)) (pos? (up y)))
                                                   (make-interval (min (* (up x) (low y))
                                                                       (* (low x) (up y)))
                                                                  (max (* (up x) (up y))
                                                                       (* (low x) (low y)))))
                                                  (else (error "bad interval"))))
        ((and (neg? (low x)) (neg? (up x))) (cond ((and (neg? (low y)) (neg? (up y)))
                                                   (make-interval  (* (up x) (up y)) (* (low x) (low y))))
                                                  ((and (neg? (low y)) (pos? (up y)))
                                                   (make-interval (* (low x) (up y)) (* (up x) (low y))))
                                                  ((and (pos? (low y)) (pos? (up y)))
                                                   (make-interval (* (low x) (up y)) (* (up x) (low y))))
                                                  (else "bad interval")))
        (else (error "bad interval"))))))))
