;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 4 Section 1 The Metacircular Evaluator

;;;; Exercise 4.9

;; Implementation of a for-loop style iteration construct
;; This is pretty ugly looking and definitely motivates the back-quote syntax used
;; in macros

;; example use, this code:
(for i 1 10 (display i))

;; ... is transformed into this, and then evaluated:
((lambda ()
   (define (loop i n)
     (if (or (< i n) (= i n))
	 (begin (display i) (loop (+ 1 i) n))))
   (loop 1 10)))

(define (eval-for exp env)
  (eval (for->combination exp) env))

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
