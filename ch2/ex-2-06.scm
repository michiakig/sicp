; Church numerals in Scheme... 
; I am continually delighted by the depth of SICP. This is the second section 
; of Chapter 2, something like the 50th exercise?
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
(add-1 one)
(add-1 (lambda (f) (lambda (x) (f x))))
(lambda (f) (lambda (x) (f ( ((lambda (f) (lambda (x) (f x))) f) x))))
(lambda (f) (lambda (x) (f ( (lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x))))

; Direct definition of addition:
