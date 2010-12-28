;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 4 Section 1 The Metacircular Evaluator

;;;; Exercise 4.14

;; Using map from the underlying Lisp doesn't work because procedures are
;; represented in the metacircular evaluator differently from the underlying Lisp:

> (lambda (x) (* x x))
(procedure (x) (* x x) <env>)

;; This doesn't mean anything to the underlying map.

;; To show that this is what is happening, one can add the underlying map as a 
;; primitive and try this:

> (map (cadr -) '(1 2 3))
(-1 -2 -3)

;; This works, because when (cadr -) is evaluated, - is evaluated to a list:

> -
(primitive #[arity-dispatched-procedure 15])

> (cadr -)
#[arity-dispatched-procedure 15]

;; The object above is what the underlying map is looking for.
