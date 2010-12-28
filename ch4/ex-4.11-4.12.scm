;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 4 Section 1 The Metacircular Evaluator

;;;; Exercise 4.11

;;; Represent a frame as an alist
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

;;;; Exercise 4.12

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

