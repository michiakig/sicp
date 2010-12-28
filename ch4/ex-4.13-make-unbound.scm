;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 4 Section 1 The Metacircular Evaluator

;;;; Exercise 4.13

;; make-unbound
;; this only removes the binding the first frame of the enviroment.
;; it doesn't seem to make sense to allow an unbind! special form to
;; remove bindings in the enclosing environments, since that could wreak
;; quite a bit of havoc?

(define (make-unbound! var env)
  (set-car! env
	    (remove! (lambda (record)
		       (eq? var (car record)))
		     (first-frame env))))

(define (eval-unbind exp env)
  (make-unbound! (car (operands exp)) env))
