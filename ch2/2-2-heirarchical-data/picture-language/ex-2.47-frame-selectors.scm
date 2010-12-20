;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 2 Section 2 Hierarchical Data and the Closure Property

;;; Exercise 2.47.

;; constructor
;; (define (make-frame origin edge1 edge2)
;;   (list origin edge1 edge2))

;; selectors
(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

;; constructor
;; (define (make-frame origin edge1 edge2)
;;   (cons origin (cons edge1 edge2)))

;; selectors
(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame cddr)
