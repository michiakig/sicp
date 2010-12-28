;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 4 Section 1 The Metacircular Evaluator

;;;; Exercise 4.6

(define (eval-let exp env)
  (eval (let->combination exp) env))

(define (let? exp) (and (tagged-list? exp 'let)
                        ;; modified to support named lets
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
