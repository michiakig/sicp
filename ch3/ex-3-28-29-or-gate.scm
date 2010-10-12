;;;; Ex 3.28, 3.29

;;; Define an or-gate as a primitive function box, and then as a compound device,
;;; using inverters and and-gates.

(define (or-gate o1 o2 output)
  (define (or-action-proc)
    (let ((new-value
	   (logical-or (get-signal o1) (get-signal o2))))
      (after-delay or-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! o1 or-action-proc)
  (add-action! o2 or-action-proc)
  'ok)

(define (or-gate o1 o2 output)
  (and-gate (inverter o1)
	    (inverter o2)
	    output))