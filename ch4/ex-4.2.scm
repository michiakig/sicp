
;;; Ex 4.2

;; a. The problem with Louis's plan is that the predicate for determining if
;; an expression is an application simply checks if it is a pair, which would
;; result in any expression like "(define x 3)" to be taken as an application.

;; b.

(define (application? exp)
  (and (pair? exp)
       (equal? (car exp) 'call)))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

;;; Ex. 4.3 this feels kind of hacky

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	(else
	 (let ((evaluator (get-evaluator exp)))
	   (if evaluator
	       evaluator
	       (if (application? exp)
		   (apply (eval (operator exp) env)
			  (list-of-values (operands exp) env))
		   (error "Unknown expression type -- EVAL")))))))
	       

(define evaluators (list *table*))

(define (get-evaluator exp)
  (if (null? *table*)
      (begin
	(init-table)
	(lookup (car exp) *table*))
      (lookup (car exp) *table*)))

(define (put key val table)
  (set-cdr! table (cons (cons key val)
			(cdr table))))

(define (init-table)
  (put 'quoted (lambda (exp env)
		 (text-of-quotation exp))
       evaluators)
  (put 'set! eval-assignment evaluators)
  (put 'define eval-definition evaluators)
  (put 'if eval-if evaluators)
  (put 'lambda (lambda (exp env)
		 (make-procedure (lambda-parameters exp)
				 (lambda-body exp)
				 env))
       evaluators)
  (put 'begin (lambda (exp env)
		(eval-sequence (begin-actions exp) env))
       evaluators)
  (put 'cond (lambda (exp env)
	       (eval (cond->if exp) env))
       evaluators)
  'done)


