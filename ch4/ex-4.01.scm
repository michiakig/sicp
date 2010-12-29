;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 4 Section 1 The Metacircular Evaluator

;;;; Exercise 4.1

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
