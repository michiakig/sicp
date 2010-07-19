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

; Exercise 2.57 Extend the differentiation program to handle sums and products 
; of arbitrary numbers of (two or more) terms.
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
        ((and (sum? a1) (sum? a2)) (collapse + '+ 0 a1 a2))
        (else (list '+ a1 a2))))

; These procedures are not entirely within the scope of the exercise above (adding 
; support for arbitrary sums and products) but I got distracted by exploring the 
; way that expressions can be reduced by collecting like terms.

; collapse two sums/products, adding/multiplying all constants together
(define (collapse op sym init s1 s2)
  (let ((not-number? (lambda (x) (not (number? x)))))
    (cons sym 
          (cons (op (accumulate op init (filter number? s1)) ; filter out the numbers, sum them, add sums together
                    (accumulate op init (filter number? s2)))
                (common-terms (append (filter not-number? (cdr s1))       ; filter out the non-numbers
                                      (filter not-number? (cdr s2)))))))) ; append them together
; collects like terms into products:
; takes a list like (a a a b b c) and turns it into ((* 3 a) (* 2 b) (* 1 c))
(define (common-terms s)
  (fold-left
      (lambda (result item)
        (cond ((null? result)
               (list (list '* 1 item)))
              ((eq? (caddar result) item)
               (cons
                 (list '* (+ 1 (cadar result)) item)
                 (cdr result)))
              (else
                (cons (list '* 1 item )
                      result))))
      '()
      (sort s (lambda (s1 s2) (string<? (symbol->string s1) (symbol->string s2))))
        ))

; some basic functional procedures from 2.2 which are used above
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; Predicate, selector, and constructor for products, with changes for exercise
; 2.57 -- products of arbitrary length
(define (product? x)
  (and (pair? x) (eq? (car x) '*))) ; the predicate only checks if the car of a list is *
(define (multiplier p) (cadr p))    ; the multiplier is also unchanged
;(define (multiplicand p) (caddr p))
(define (multiplicand p) ;(cons '* (cddr p)))
  (cond ((null? (cdddr p)) (caddr p))
        (else (cons '* (cddr p)))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (collapse * '* 1 (cdr m1) (cdr m2)))))
