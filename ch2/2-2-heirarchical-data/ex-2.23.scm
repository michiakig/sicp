;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 2 Section 2 Hierarchical Data and the Closure Property

;;; Exercise 2.23

(define for-each
  (lambda (f l)
    (cond ((null? l) #t)
          (else (f (car l)) ; This is sort of weird, I think it's the first
                            ; time I've used this more imperative style
                (for-each f (cdr l)))))) ;
