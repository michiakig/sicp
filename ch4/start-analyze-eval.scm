;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 4 Section 1 The Metacircular Evaluator

(load "/Users/aki/hacking/structure-and-interpretation/ch4/ch4-mceval.scm")
(load "/Users/aki/hacking/structure-and-interpretation/ch4/ch4-analyzingmceval.scm")
(load "/Users/aki/hacking/structure-and-interpretation/ch4/ex-4.22-analyze-let.scm")

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
;(driver-loop)


