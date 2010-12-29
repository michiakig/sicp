;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 4 Section 1 The Metacircular Evaluator

;;;; Exercise 4.20

(define (eval-letrec exp env)
  (eval (letrec->let exp) env))

(define (letrec-bindings exp) (cadr exp))
(define (letrec-body exp) (cddr exp))
(define (binding-var binding) (car binding))
(define (binding-val binding) (cadr binding))

(define (letrec->let exp)
  (append (list 'let
		(map (lambda (binding)
		       (list (binding-var binding)
			     ''*unassigned*))
		     (letrec-bindings exp)))
	  (map (lambda (binding)
		 (list 'set!
		       (binding-var binding)
		       (binding-val binding)))
	       (letrec-bindings exp))
	  (letrec-body exp)))
	     
