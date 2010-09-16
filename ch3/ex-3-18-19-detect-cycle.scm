;; Exercise 3.18
;; Write a procedure that examines a list and determines whether it contains a cycle.

(define (detect-cycle x)
  (define seen '())
  (define (detect-cycle-r x)
    (cond ((null? x) #f)
	  ((memq x seen) #t)
	  (else
	   (set! seen (cons x seen))
	   (detect-cycle-r (cdr x)))))
  (detect-cycle-r x))

;; Exercise 3.19
;; Redo exercise 3.18 using an algorithm using only a contant amount of space.

; Floyd's famous tortoise and hare algorithm
(define (detect-cycle-constant x)
  (define (floyd t h)
    (cond ((null? h) #f)
	  ((null? t) #f)
	  ((eq? t h) #t) ; hare lapped the tortoise?
	  (else
	   (floyd (cdr t) (cddr h)))))
  (floyd x (cdr x)))