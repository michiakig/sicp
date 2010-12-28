;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 4 Section 1 The Metacircular Evaluator

;;;; Exercise 4.5

;;; I'm not really sure how to do this without using let, so it's assumed that let 
;;; is already implemented.

;> (cond ((assoc 'b '((a 1) (b 2))) => cadr)
;	(else false))
;2

(define (cond-extended? clause) (equal? '=> (cadr clause)))
(define (cond-test clause) (car clause))
(define (cond-recipient clause) (caddr clause))

(define (extended-clause->if clause rest)
  (list 'let
	(list (list 'val (cond-test clause)))
	(make-if 'val
		 (list (cond-recipient clause) 'val)
		 (expand-clauses rest))))


(define (clause->if clause rest)
  (make-if (cond-predicate clause)
	   (sequence->exp (cond-actions clause))
	   (expand-clauses rest)))

(define (expand-clauses clauses)
  (cond ((null? clauses) 'false)
	((cond-else-clause? (car clauses))
	 (if (null? (cdr clauses))
	     (sequence->exp (cond-actions (car clauses)))
	     (error "ELSE clause isn't last -- COND->IF"
		    clauses)))
	((cond-extended? (car clauses))
	 (extended-clause->if (car clauses) (cdr clauses)))
	(else (clause->if (car clauses) (cdr clauses)))))


