;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 4 Section 1 The Metacircular Evaluator

;;;; Exercise 4.3

;;; eval written in a data-directed style, dispatching on the type of expression

(define (get-type exp)
  (cond ((not (pair? exp))
	 (cond ((self-evaluating? exp) 'self-evaluating)
	       ((variable? exp) 'variable)))
	(else 
	 (car exp))))

;; evaluation procedures for those special forms which don't have appropriate ones in the text
(define (eval-quotation exp env)
  (text-of-quotation exp))

(define (eval-lambda exp env)
  (make-procedure (lambda-parameters exp)
		  (lambda-body exp)
		  env))

(define (eval-begin exp env)
  (eval-sequence (begin-actions exp) env))

(define (eval-cond exp env)
  (eval (cond->if exp) env))

(define (eval-self exp env)
  exp)

(define (eval-variable exp env)
  (lookup-variable-value exp env))

;; alist of special form name -> evaluation procedure take two args, the expression and the environment just like the eval-* procedures in the text
(define *table* (list (cons 'quote eval-quotation)
		      (cons 'set! eval-assignment)
		      (cons 'define eval-definition)
		      (cons 'if eval-if)
		      (cons 'lambda eval-lambda)
		      (cons 'begin eval-begin)
		      (cons 'cond eval-cond)
		      (cons 'self-evaluating eval-self)
		      (cons 'variable eval-variable)

		      ;; additional procedures
		      (cons 'and eval-and)
		      (cons 'or eval-or)
		      (cons 'let eval-let)
		      (cons 'let* eval-let*)
		      ))

;; data-directed eval
(define (eval exp env)
  (let ((record (assoc (get-type exp) *table*)))
    (cond (record ((cdr record) exp env))
	  ((application? exp)
	   (apply (eval (operator exp) env)
		  (list-of-values (operands exp) env)))
	  (else
	   (error "Unknown expression type -- EVAL" exp)))))