;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 2 Section 2 Hierarchical Data and the Closure Property

;;; Exercise 2.29

; defined for us
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

; a
(define (left-branch m) (car m))
(define (right-branch m) (cadr m))
(define (branch-length b) (car b))
(define (branch-structure b) (cadr b))

; b
; After defining the selectors above in such a general way, I made a mistake
; testing the total-weight function below. I constructed the test mobile
; incorrectly: the top level mobile did not have a branch for the right, 
; instead it was just another mobile, without a lenght. Scheme's weak typing 
; lets you create these... I would guess that Haskell would not have allowed
; that test to be constructed.
(define total-weight
  (lambda (m)
    (cond ((not (pair? m)) m)
          (else (+ (total-weight (branch-structure
                                   (left-branch m)))
                   (total-weight (branch-structure
                                   (right-branch m))))))))

; c

(define balanced?
  (lambda (m)
    (cond ((not (pair? m)) #t)
          (else (and (= (branch-torque (left-branch m))
                        (branch-torque (right-branch m)))
                     (balanced? (branch-structure
                                  (left-branch m)))
                     (balanced? (branch-structure
                                  (right-branch m))))))))

(define branch-torque
  (lambda (b)
    (* (branch-length b)
       (total-weight (branch-structure b)))))

; d
; just need to change selectors:
(define left-branch car)
(define right-branch cdr)
(define branch-length car)
(define branch-structure cdr)
