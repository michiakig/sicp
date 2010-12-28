;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 4 Section 1 The Metacircular Evaluator

;;;; Exercise 4.6

(define (eval-let exp env)
  (cond ((let? exp) (eval (let->combination exp) env))
	((named-let? exp) (eval (named-let->combination exp) env))
	(else (error "EVAL-LET let form is neither let nor named let" exp))))

(define (let? exp)
  (and (tagged-list? exp 'let)
       (list? (cadr exp)))) ; make sure it's not a named let

(define (let-body exp) (cddr exp))
(define (let-bindings exp) (cadr exp))

(define (let->combination exp)
  (define (loop rest vars exps body)
    (if (null? rest)
	;; we can either reverse before passing to make-lambda
	(cons (make-lambda (reverse vars) body) (reverse exps))
	(loop (cdr rest)
	      ;; or do something ugly like (append vars (list (caar rest)))
	      ;; the goal is to keep the argument and expression list the same
	      ;; as in the let, because otherwise you would effectively reverse
	      ;; the evaluation order for the values which are being bound to
	      (cons (caar rest) vars)
	      (cons (cadar rest) exps)
	      body)))
  (loop (let-bindings exp) '() '() (let-body exp)))

;;;; Exercise 4.7

(define (eval-let* exp env) (eval (let*->nested-lets exp) env))

;; It's perfectly fine to define let* in this way because when 
;; (eval (let*->nested-lets exp) env) is called it will recursively 
;; call eval again, with the nested let expression, then with the lets
;; expanded to be lambda applications, etc ...

(define (let*->nested-lets exp)
  (define (rec bindings body)
    (if (null? (cdr bindings))
	(cons 'let (cons (list (car bindings)) body))
	(list 'let (list (car bindings)) (rec (cdr bindings) body))))
  (rec (let-bindings exp) (let-body exp)))

;;;; Exercise 4.8

;; support for named let form
;; named lets are differentiated from normal lets by the eval-let procedure above

(define (named-let? exp) (and (tagged-list? exp 'let)
			      (not (pair? (let-bindings exp)))))

(define (named-let-name exp) (cadr exp))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-body exp) (cdddr exp))

(define (named-let->combination exp)
  (define (nloop bindings vars exps body)
    (if (null? bindings)
	(list (make-lambda '()
			   (list (list 'define
				       (named-let-name exp)
				       (make-lambda (reverse vars) body))
				 (cons (named-let-name exp) (reverse exps)))))
	(nloop (cdr bindings)
	       (cons (caar bindings) vars)
	       (cons (cadar bindings) exps)
	       body)))
  (nloop (named-let-bindings exp) '() '() (named-let-body exp)))
