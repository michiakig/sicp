(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

; Exercise 2.8
(define sub-interval
  (lambda (x y)
    (make-interval (- (upper-bound x) (upper-bound y))
                   (- (lower-bound x) (lower-bound y)))))

; Exercise 2.9
(define width
  (lambda (x)
    (/ (abs (- (upper-bound x)
               (lower-bound x)))
       2)))

; Exercise 2.10
(define (div-interval x y)
  (if (= 0 (width y))
      (error "divide by 0")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))
; Exercise 2.12
(define make-center-percent
  (lambda (c p)
    (let ((w (/ (* c p) 100.0)))
      (make-interval (- c w)
                     (+ c w)))))
