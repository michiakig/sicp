
;;; Ex. 4.1

;; This version of list-of-values evaluates operands from left-to-right.
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((val (eval (first-operand exps) env)))
	(cons val
	      (list-of-values (rest-operands exps) env)))))

;; This version evaluates the operands from right-to-left.
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values (rest-operands exps) env)))
	(cons (eval (first-operand exps) env)
	      rest))))

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

;; Ex. 4.4

(define eval-and (operands env)
  (if (null? (cdr operands)) ; last one
      (eval (car operands) env)
      (if (eval (car operands) env)
	  (eval-and (cdr operands) env)
	  #f)))

(define eval-or (operands env)
  (if (null? (cdr operands))
      (eval (car operands) env)
      (let ((val (eval (car operands) env)))
	(if val
	    val
	    (eval-or (cdr operands) env)))))

;; Ex. 4.5

(define (cond-action clause) (caddr clause))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
	    ;; the expand-clauses is the same as in the text up until here
	    ;; we compute the value of the predicate and store it in val
	    ;; and use this to create an if 
	      (make-if (cond-predicate first)
		       (list (cond-action first) (cond-predicate first))
		       (expand-clauses rest))))))

;; Ex. 4.6

(define (let? exp)
  (equal? (car exp) 'let))

(define (let->combination exp)
  (define (rec rest vars exps body)
    (if (null? vars)
	(cons (make-lambda vars body) exps)
	(rec (cdr rest)
	     (cons (caar rest) vars)
	     (cons (cadar rest) exps)
	     body)))
  (rec (cadr exp) '() '() (cddr exp)))

