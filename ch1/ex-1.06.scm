;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 1 Section 1 Elements of Programming

;;; Exercise 1.06

; Alyssa P. Hacker doesn't see why if needs to be provided as a special form. ``Why can't I just define it as an ordinary procedure in terms of cond?'' she asks. Alyssa's friend Eva Lu Ator claims this can indeed be done, and she defines a new version of if:
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; Delighted, Alyssa uses new-if to rewrite the square-root program:
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
; What happens when Alyssa attempts to use this to compute square roots? Explain.
;
; A: Infinite loop, because of Scheme's applicative-order evaluation, (sqrt-iter (improve guess x) x) is
; evaluated before new-if is completely evaluated, resulting in an infinite loop.

(define (improve guess x) (average guess (/ x guess)))
(define (average x y) (/ (+ x y) 2))
