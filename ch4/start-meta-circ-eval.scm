;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 4 Section 1 The Metacircular Evaluator

;;; This is an init file which loads the basic metacircular evaluator file,
;;; and then sets up and starts the REPL.

;; this file contains source from the text
;; definitions loaded after this one may re-define certain things
(load "/Users/aki/hacking/structure-and-interpretation/ch4/meta-circ-eval.scm")

(load "/Users/aki/hacking/structure-and-interpretation/ch4/ex-4.4-and-or.scm")
(load "/Users/aki/hacking/structure-and-interpretation/ch4/ex-4.5-extended-cond.scm")
(load "/Users/aki/hacking/structure-and-interpretation/ch4/ex-4.6-4.7-4.8-let-star-named.scm")
(load "/Users/aki/hacking/structure-and-interpretation/ch4/ex-4.9-iteration.scm")
(load "/Users/aki/hacking/structure-and-interpretation/ch4/ex-4.11-4.12.scm")
(load "/Users/aki/hacking/structure-and-interpretation/ch4/ex-4.13-make-unbound.scm")
(load "/Users/aki/hacking/structure-and-interpretation/ch4/ex-4.16-internal-defines.scm")

;; data-directed version of eval
(load "/Users/aki/hacking/structure-and-interpretation/ch4/ex-4.3-data-direct.scm")

;(trace eval)
;(trace apply)

;; add some primitives to make various testing easier
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)

;;      more primitives
	(list '+ +)
	(list '- -)
	(list '* *)
	(list '/ /)
	(list '= =)
	(list '> >)
	(list '< <)
	(list '>= >=)
	(list '<= <=)
	(list 'eq? eq?)
	(list 'equal? equal?)
	(list 'display display)
	(list 'cadr cadr)
        ))

(define the-global-environment (setup-environment))

;; for exercise 4.5
(eval '(define (assoc val alist)
	 (if (null? val)
	     #f
	     (if (equal? val (car (car alist)))
		 (car alist)
		 (assoc val (cdr alist)))))
      the-global-environment)

;; for exercise 4.14
(eval '(define (map fn lst)
	 (if (null? lst)
	     '()
	     (cons (fn (car lst))
		   (map fn (cdr lst)))))
      the-global-environment)

;(driver-loop)
