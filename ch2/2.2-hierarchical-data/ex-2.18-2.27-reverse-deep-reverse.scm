;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 2 Section 2 Hierarchical Data and the Closure Property

;;; Exercise 2.18 and 2.27

(define (reverse l)
  (define (reverse-iter l rev)
    (cond ((null? (cdr l)) (cons (car l) rev))
          (else (reverse-iter (cdr l) (cons (car l) rev)))))
  (reverse-iter l '()))
    
(define (deep-reverse l)
  (define (deep-rev-iter l rev)
    (cond ((null? l) rev)
          ((pair? (car l))
           (deep-rev-iter (cdr l)
                          (cons (deep-rev-iter (car l) '()) rev)))
          (else (deep-rev-iter (cdr l) (cons (car l) rev)))))
  (deep-rev-iter l '()))
