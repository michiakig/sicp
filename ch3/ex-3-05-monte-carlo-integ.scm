;; Exercise 3.5 Monte Carlo integration

(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((area (* (abs (- x1 x2)) ; area of the rectangle
                 (abs (- y1 y2)))))
    (* area (monte-carlo trials (lambda () ; tests a random point (x,y)
                                  (P (random-in-range x2 x1)
                                     (random-in-range y2 y1)))))))

;; Returns a predicate that checks if a given point is in the defined circle
(define (get-circle-p center-x center-y radius)
  (lambda (x y)
    (<= (dist center-x x center-y y) radius)))

;; calculate the distance between two points
(define (dist x0 x1 y0 y1)
  (sqrt (+ (square (- x1 x0))
           (square (- y1 y0)))))

; Providing in the text:
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

;; Exercise 3.6 reset-able random number generator

(define rand
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate)
             (set! x (rand-update x))
             x)
            ((eq? m 'reset)
             (lambda (new-value)
               (set! x (rand-update new-value))))
            (else (error "bad message?" m))))))
