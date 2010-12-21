
;;;; METACIRCULAR EVALUATOR FROM CHAPTER 4 (SECTIONS 4.1.1-4.1.4) of
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;; Exercises interleaved with code from the text
;;;; 4.1, 4.4, 4.5, 4.6, 4.7, 4.8, 4.9, 4.11, 4.12, 4.13, 4.16

;;;; Matches code in ch4.scm

;;;; This file can be loaded into Scheme as a whole.
;;;; Then you can initialize and start the evaluator by evaluating
;;;; the two commented-out lines at the end of the file (setting up the
;;;; global environment and starting the driver loop).

;;;; **WARNING: Don't load this file twice (or you'll lose the primitives
;;;; interface, due to renamings of apply).

;;; from section 4.1.4 -- must precede def of metacircular apply
(define apply-in-underlying-scheme apply)

;;;SECTION 4.1.1

(define (eval exp env)

;  (if (pair? exp)
;      (begin
;	(display "EVAL:")
;	(display exp)
;	(newline)))

  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))

	((and? exp) (eval-and (operands exp) env))
	((or? exp) (eval-or (operands exp) env))
	((let? exp) (eval (let->combination exp) env))
	((let*? exp) (eval (let*->nested-lets exp) env))
	((named-let? exp) (eval (named-let->combination exp) env))
	((for? exp) (eval (for->combination exp) env))
	((unbind? exp) (eval-unbind exp env))

        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "**** Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)

;  (display "APPLY:")
;  (newline)

  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "**** Unknown procedure type -- APPLY" procedure))))

;;; Ex. 4.1

;; This version of list-of-values evaluates operands from left-to-right.

;; Add "display" to the primitives and used this test the evaluation order:
;; (cons (begin (display "A") 1) (begin (display "B") 2))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((val (eval (first-operand exps) env)))
	(cons val
	      (list-of-values (rest-operands exps) env)))))

;; This version evaluates the operands from right-to-left.

;(define (list-of-values exps env)
;  (if (no-operands? exps)
;      '()
;      (let ((rest (list-of-values (rest-operands exps) env)))
;	(cons (eval (first-operand exps) env)
;	      rest))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;;; Ex. 4.4

(define (eval-and ops env)
  (if (null? (cdr ops)) ; last one
      (eval (car ops) env)
      (if (eval (car ops) env)
	  (eval-and (cdr ops) env)
	  false)))

(define (eval-or ops env)
  (if (null? (cdr ops))
      (eval (car ops) env)
      (let ((val (eval (car ops) env)))
	(if val
	    val
	    (eval-or (cdr ops) env)))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;;;SECTION 4.1.2

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (if? exp) (tagged-list? exp 'if))

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

;;; Ex. 4.5

(define (cond-action clause) (caddr clause))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "**** ELSE clause isn't last -- COND->IF"
                       clauses))
	    ;; the expand-clauses is the same as in the text up until here
	    ;; we compute the value of the predicate and store it in val
	    ;; and use this to create an if 
	      (make-if (cond-predicate first)
		       (list (cond-action first) (cond-predicate first))
		       (expand-clauses rest))))))

;;; Ex. 4.6

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

;;; Ex. 4.7

(define (let*? exp) (tagged-list? exp 'let*))

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

;; Ex. 4.8 support for named let form
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

;; Ex. 4.9 Implementation of an for-loop style iteration construct
;; This is pretty ugly looking and definitely motivates the back-quote
;; syntax used in macros, etc.

(define (for? exp) (tagged-list? exp 'for))
(define (for-var exp) (cadr exp))
(define (for-init exp) (caddr exp))
(define (for-limit exp) (cadddr exp))
(define (for-body exp) (cddddr exp))

(define (for->combination exp)
  (list
   (make-lambda '()
		(list (list
		       'define
		       (list 'loop (for-var exp) 'n)
		       (list 'if
			     (list 'or
				   (list '< (for-var exp) 'n)
				   (list '= (for-var exp) 'n))
			     (cons 'begin
				   (append (for-body exp)
					   (list (list 'loop
						       (list '+ 1 (for-var exp))
								 'n))))))
		      (list 'loop (for-init exp) (for-limit exp))))))

;;;SECTION 4.1.3

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


(define (make-procedure parameters body env)
  ;; changed for ex. 4.16 (c), installed here so it runs when the
  ;; procedure is created, not when it is applied 
  (list 'procedure parameters (scan-out-defines body) env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

;; Ex. 4.11 Represent a frame as an alist
(define (make-frame variables values)
  (map cons variables values))

(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))

(define (first-binding frame) (car frame))
(define (rest-binding frame) (cdr frame))

;; now we can use assoc very simply to look up a binding
(define (find-in-frame var frame) (assoc var frame))

;; this is basically (push (cons var val) frame)
(define (add-binding-to-frame! var val frame)
  (let ((f (cons (car frame) (cdr frame))))
    (set-car! frame (cons var val))
    (set-cdr! frame f)))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "**** Too many arguments supplied" vars vals)
          (error "**** Too few arguments supplied" vars vals))))

;; now we can just use assoc instead of the internal scan procedures
(define (lookup-variable-value var env)
  (let ((record (env-frame-search var env)))
    (if record
	;; changed to support ex. 4.16 (a)
	(if (eq? (cdr record) '*unassigned*)
	    (error "Unassigned variable" var)
	    (cdr record))
	(error "Unbound variable" var))))

(define (set-variable-value! var val env)
  (let ((record (env-frame-search var env)))
    (if record
        (set-cdr! record val)
        (error "**** Unbound variable -- SET!" var))))
	
(define (define-variable! var val env)
  (let ((record (find-in-frame var (first-frame env))))
    (if record
	(set-cdr! record val)
	(add-binding-to-frame! var val (first-frame env)))))

;; Ex. 4.12
;; this searches the given environment, and it's enclosing environments,
;; for the given variable, return the pair (var . val) or false
;; this still doesn't really implement much more than a leaky abstraction
;; to improve readability...

(define (env-frame-search var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
	#f
	(let ((record (find-in-frame var (first-frame env))))
	  (if record
	      record
	      (env-loop (enclosing-environment env))))))
  (env-loop env))

;; Ex. 4.13 make-unbound
;; this only removes the binding the first frame of the enviroment.
;; it doesn't seem to make sense to allow an unbind! special form to
;; remove bindings in the enclosing environments, since that could wreak
;; quite a bit of havoc?

(define (make-unbound! var env)
  (set-car! env
	    (remove! (lambda (record)
		       (eq? var (car record)))
		     (first-frame env))))

(define (unbind? exp) (tagged-list? exp 'unbind!))

(define (eval-unbind exp env)
  (make-unbound! (car (operands exp)) env))

;;;SECTION 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

;[do later] (define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)

;;      arithmetic primitives
	(list '+ +)
	(list '- -)
	(list '* *)
	(list '/ /)
	(list '< <)
	(list '> >)
	(list '= =)

;;      more primitives
	(list 'display display)
	(list 'newline newline)

	(list 'append append)
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;[moved to start of file] (define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;;; SECTION 4.1.6 Internal Definitions

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

;; Ex. 4.16 (b) - implementation of the method for interpreting
;; internal definitions
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

;;;Following are commented out so as not to be evaluated when
;;; the file is loaded.
(define the-global-environment (setup-environment))
(driver-loop)




;;'METACIRCULAR-EVALUATOR-LOADED
