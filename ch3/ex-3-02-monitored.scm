;; Exercise 3.2

(define (make-monitored f)
  (let ((n 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) n)
            ((eq? x 'reset-count) (set! n 0))
            (else
              (set! n (+ n 1))
              (f x))))))
            

