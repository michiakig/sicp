;;;; Initialize the generic arithmetic system

;;; This function loads the required files and calls the installation 
;;; procedures.

(define (init)
  (load "rect-and-polar-pckgs.scm")
  (load "generic-arith.scm")
  (load "apply-generic.scm")
  (load "proto-table.scm")
  (install-rectangular-package)
  (install-polar-package)
  (install-scheme-number-package)
  (install-rational-package)
  (install-complex-package)
  'done!)

