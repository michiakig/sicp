;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 4 Section 1 The Metacircular Evaluator

;;;; Exercise 4.4

(define (eval-and exp env)
  (define (r ops env)
    (if (null? (cdr ops)) ; last one
	(eval (car ops) env)
	(if (eval (car ops) env)
	    (r (cdr ops) env)
	    false)))
  (r (cdr exp) env))

(define (eval-or exp env)
  (define (r ops env)
    (if (null? (cdr ops))
	(eval (car ops) env)
	(let ((val (eval (car ops) env)))
	  (if val
	      val
	      (r (cdr ops) env)))))
  (r (cdr exp) env))
