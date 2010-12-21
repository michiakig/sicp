;;;; Exercise 4.25

;; from the text:

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

;; If this procedure were evaluated in an applicative-order Scheme,
;; it would enter an infinite loop, since each call to `unless` would
;; evaluate its arguments, including a recursive call to `factorial`.

;; In a normal-order Scheme, this would work.

;;;; Exercise 4.26

(define (unless-condition exp) (cadr exp))
(define (unless-usual exp) (caddr exp))
(define (unless-exceptional exp) (cadddr exp))

(define (eval-unless exp env)
  (eval (list 'if
	      (unless-condition exp)
	      (unless-exceptional exp)
	      (unless-usual exp))))

;; I cannot fathom when it would be useful to use unless as an argument to a higher-order procedure

;;;; Exercise 4.27

(define count 0)

(define (id x)
  (set! count (+ count 1))
  x)

(define w (id (id 10)))

count
;; 0
w
;; 10
count
;; 2

;; I was actually wrong above, my intuition was that in the definition
;; (define w (id (id 10))), (id (id 10)) would be delayed, and so count
;; would be 0. But the definition is evaluated, which results in the
;; definition value is evaluated, and this is an application, so the
;; inner call to id is delayed but the outer is not.

(define (square x)
  (* x x))

(square (id 10))

;; 100

count

;; When the evaluator uses memoization, count will be 1, and when it
;; doesn't it will be 2.
