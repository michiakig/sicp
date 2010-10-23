;;;; Ex 3.33

;;; using primitive multiplier, adder, and constant constraints, define an averager

(define (averager a b c)
  (let ((x (make-connector))
	(y (make-connector))
	(z (make-connector)))
    (adder a b x)
    (multiplier x z c)
    (constant 1/2 z)
    'ok))