;#lang scheme

; Exercise 2.56.  Show how to extend the basic differentiator to handle more kinds of expressions.
; For instance, implement the differentiation rule [power rule]

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))

        ((exponentiation? exp) ; new clause to implement power rule
         (make-product
           (exponent exp)
           (make-exponentiation (base exp) (- (exponent exp) 1))))

        (else
         (error "unknown expression type -- DERIV" exp))))

; predicate, selector, constructor for exponentiation
(define (exponentiation? exp) (and (pair? exp) (eq? (car exp) '**)))
(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))
(define (make-exponentiation base exponent)
  (cond ((= exponent 0) 1)
        ((= exponent 1) base)
        (else (list '** base exponent))))

; selectors and constructurs:
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

; Exercise 2.57 Extend the differentiation program to handle sums and products of arbitrary numbers of (two or more) terms.
(define (sum? x) 
  (and (pair? x) (eq? (car x) '+)))       ; No need to change sum? because this only checks if the car of a list is +
(define (addend s) (cadr s))              ; Can also leave addend alone
;(define (augend s) (caddr s))            ; original augend
(define (augend s)
  (cond ((null? (cdddr s))
         (caddr s))
        (else (cons '+ (cddr s)))))

; We need to change make-sum to collapse two sums into one:
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((and (sum? a1) (not (sum? a2))) (cons '+ (cons a2 (cdr a1)))) 
        ((and (sum? a2) (not (sum? a1))) (cons '+ (cons a1 (cdr a2))))
        ((and (sum? a1) (sum? a2)) (collapse-sums a1 a2))
        (else (list '+ a1 a2))))

; collapse two sums, adding all constants together
(define (collapse-sums s1 s2)
  (let ((not-number? (lambda (x) (not (number? x)))))
    (cons '+
          (cons (+ (accumulate + 0 (filter number? s1)) ; filter out the numbers, sum them, add sums together
                   (accumulate + 0 (filter number? s2)))
                (append (filter not-number? s1)   ; filter out the non-numbers
                        (filter not-number? s2)))))) ; append them together

; need this for collapse-sum
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

; exercise 2.58
