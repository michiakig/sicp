;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 2 Section 4 Multiple Representations for Abstract Data
;;;; Chapter 2 Section 5 Systems with Generic Operations

;;;; Initialize the generic arithmetic system

;;; This function loads the required files and calls the installation 
;;; procedures.

(define (init)
  (load "rect-and-polar-pckgs.scm")
  (load "generic-arith.scm")
  (load "apply-generic.scm")
  (load "proto-table.scm")
  (load "symbolic-algebra.scm")
  (install-rectangular-package)
  (install-polar-package)
  (install-scheme-number-package)
  (install-rational-package)
  (install-complex-package)
  (install-polynomial-package)
  'done!)

