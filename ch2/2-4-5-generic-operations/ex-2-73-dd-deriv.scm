; Exercise 2.73 
; Impelement symbolic differentiation from sec. 2.3.2, using the data-directed
; style described in this section. Below is the modified deriv procedure using
; which dispatches on the operator of an expression.

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; b. Procedures for sums and products and code to install them in the table
; These are not updated with the extra code to handle arbitrary sums and products
; c. Added procedure to handle exponentiation
; d. The install-deriv procedure below would need to be changed,
; (put '+ 'deriv deriv-of-sum) etc...
(define (install-deriv)
  (define (deriv-of-sum operands var) 
    (make-sum (deriv (car operands) var)
              (deriv (cadr operands) var)))
  (define (deriv-of-product operands var)
    (make-sum (make-product (car operands)
                            (deriv (cadr operands) var))
              (make-product (deriv (car operands) var)
                            (cadr operands))))
  (define (deriv-of-exp operands var)
    (make-product
      (cadr operands)
      (make-exponentiation (car operands) (- (cadr operands) 1))))
  (put 'deriv '+ deriv-of-sum)
  (put 'deriv '* deriv-of-product)
  (put 'deriv '** deriv-of-exp))

