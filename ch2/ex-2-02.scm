(define make-point (lambda (x y) (cons x y)))
(define x-point (lambda (point) (car point)))
(define y-point (lambda (point) (cdr point)))

(define make-segment make-point)
(define start-segment x-point)
(define end-segment y-point)

(define midpoint-segment
  (lambda (segment)
    (make-point (/ (+ (x-point (start-segment segment))
                      (x-point (end-segment segment)))
                   2)
                (/ (+ (y-point (start-segment segment))
                      (y-point (end-segment segment)))
                   2))))

; provided by the exercise...
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
