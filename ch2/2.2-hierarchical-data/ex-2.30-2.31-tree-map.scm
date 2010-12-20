;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 2 Section 2 Hierarchical Data and the Closure Property

;;; Exercise 2.30 and 2.31

(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (cond ((null? sub-tree) '())
               ((pair? sub-tree) (square-tree sub-tree))
               (else (* sub-tree sub-tree))))
       tree))

(define (tree-map f tree)
  (map (lambda (sub-tree)
         (cond ((null? sub-tree) '())
               ((not (pair? sub-tree)) (f sub-tree))
               (else (tree-map f sub-tree))))
       tree))
