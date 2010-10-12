;;;; Ex. 3.14, 3.17

;;; A better name for mystery would be reverse! It destructively (but 
;;; efficiently) reverses a list.

(define (reverse! x)
  (define (loop x y)
    (if (null? x)
        y
      (let ((temp (cdr x)))
        (set-cdr! x y)
        (loop temp x))))
  (loop x '()))

;;; A correct version of count-pairs

(define (count-pairs x)
  (let ((lst '()))
    (define (count-pairs-r x)
      (cond ((not (pair? x)) 0)
	    ((not (memq x lst))
	     (set! lst (cons x lst))
	     (+ (count-pairs-r (car x))
		(count-pairs-r (cdr x))
		1))
	    (else 0)))
    (count-pairs-r x)))
