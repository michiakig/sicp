;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 4 Section 1 The Metacircular Evaluator

;;;; SECTION 4.1.6 Internal Definitions

;;;; Exercise 4.16

;; predicates and selectors for "regular" definitions like (define a 1) 
(define (define? e) (tagged-list? e 'define))
(define (define-var e) (cadr e))
(define (define-exp e) (caddr e))

;; and procedure definition syntactic sugar (define (foo x) ...)
(define (proc-define? e) (list? (cadr e)))
(define (proc-define-var e) (caadr e))
(define (proc-define-formals e) (cdadr e))
(define (proc-define-body e) (cddr e))

(define (get-exp-or-lambda e)
  (if (proc-define? e)
      (cons 'lambda
            (cons (proc-define-formals e)
                  (proc-define-body e)))
      (define-exp e)))

(define (get-var-or-proc e)
  (if (proc-define? e)
      (proc-define-var e)
      (define-var e)))


;; changed to support ex. 4.16 (a)
(define (lookup-variable-value var env)
  (let ((record (env-frame-search var env)))
    (if record
	(if (eq? (cdr record) '*unassigned*)
	    (error "Unassigned variable" var)
	    (cdr record))
	(error "Unbound variable" var))))

;; (b) - implementation of the method for interpreting internal definitions
(define (scan-out-defines body)
  (let* ((definitions (filter define? body))
	 (rest (remove define? body))
	 (vars (map get-var-or-proc definitions))
	 (exps (map get-exp-or-lambda definitions)))
    (if (null? definitions)
        body
        (list (cons 'let
                    (cons (map (lambda (v) (list v ''*unnassigned*)) vars)
                          (append (map (lambda (v e) (list 'set! v e))
                                       vars
                                       exps)
                                  rest)))))))

;; changed for ex. 4.16 (c), installed here so it runs when the
;; procedure is created, not when it is applied 
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))


