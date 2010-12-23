;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 4 Section 1 The Metacircular Evaluator

;;; This is an init file which loads the basic metacircular evaluator file,
;;; and then sets up and starts the REPL.

;; this file contains source from the text
;; definitions loaded after this one may re-define certain things
(load "/Users/aki/hacking/structure-and-interpretation/ch4/meta-circ-eval.scm")

;; data-directed version of eval
(load "/Users/aki/hacking/structure-and-interpretation/ch4/ex-4.3-data-direct.scm")

;(trace eval)
;(trace apply)

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
	(list 'eq? eq?)
	(list 'equal? equal?)
        ))

(define the-global-environment (setup-environment))
(driver-loop)
