; Church numerals in Scheme... 
; I am continually delighted by the depth of SICP. This is the second section 
; of Chapter 2, something like the 50th exercise? Awesome.
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n) (lambda (f) (lambda (x) (f ((n f) x)))))

; Derivation of the number 1, as a Church numeral
(add-1 zero)
(add-1 (lambda (f) (lambda (x) x)) )
(lambda (f) (lambda (x) (f ( ((lambda (f) (lambda (x) x))f) x))))
                              ;^^^^^^^^^^^^^^^^^^^^^^^^^^this is zero
                              ;apply this to f and result is (lambda (x) x) 
(lambda (f) (lambda (x) (f ( (lambda (x) x) x))))
                              ;apply identity to x
(lambda (f) (lambda (x) (f x))) ; I think this is all the simplifying we can do
                                ; so this is number 1 as a Church numeral.

; Derivation of 2:
; (add-1 one)
; (add-1 (lambda (f) (lambda (x) (f x))))
; (lambda (f) (lambda (x) (f ( ((lambda (f) (lambda (x) (f x))) f) x))))
; (lambda (f) (lambda (x) (f ( (lambda (x) (f x)) x))))
; (lambda (f) (lambda (x) (f (f x))))

; Direct definition of addition:
; My first thought was...
(define (add a b)
  (lambda (f)
    (lambda (x) (a ((b f) x)))))
; But actually, I believe this is multiplication.
; from Wikipedia,
; "The higher-order function that represents natural number n is a function that
; maps any other function f to its n-fold composition."
; This was very helpful. It succinctly describes the derivations above.
; It also means that given a Church numeral n in Scheme, (n f) is the n-fold
; composition of f.
; This lead to below, which I believe is correct.
;
(define (add a b)
  (lambda (f)
    (lambda (x) ((a f) ((b f) x)) )))

; (add one two)
(add (lambda (f) (lambda (x) (f x))) (lambda (f) (lambda (x) (f (f x)))))

(lambda (f)
  (lambda (x)
    (((lambda (f) (lambda (x) (f x))) f)
     (((lambda (f) (lambda (x) (f (f x)))) f) x)))) ; apply two to f and one to f

(lambda (f)
  (lambda (x)
    ((lambda (x) (f x))
     ((lambda (x) (f (f x))) x)))) ; apply (two f) to x

(lambda (f)
  (lambda (x)
    ((lambda (x) (f x)) (f (f x)) ))) ; apply (one f) to ((two f) x)

(lambda (f)
  (lambda (x)
    (f (f (f x)))))
