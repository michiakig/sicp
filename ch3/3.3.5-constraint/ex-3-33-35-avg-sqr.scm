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

;;; Fill in the missing portions in Ben's outline for a procedure to implement 
;;; a squarer as a new primitive constraint

(define (squarer a b)
  (define (process-new-value)
    (cond ((has-value? b)
	   (if (< (get-value b) 0)
	       (error "square less than 0 -- SQUARER" (get-value b))
	    (set-value! a (sqrt (get-value b)) me)))
	  ((has-value? a)
	   (set-value! b (square (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! b me)
    (forget-value! a me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
	   (process-new-value))
	  ((eq? request 'I-lost-my-value)
	   (process-forget-value))
	  (else
	   (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)
