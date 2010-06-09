(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

; This idea is similarly presented in "The Seasoned Schemer" (maybe Felleisen
; and Friedman borrow it from SICP?).  Even having seen it before, it's still
; mind-blowing.  Appropriate definition of cdr...

(define (cdr z)
  (z (lambda (p q) q)))
