;(for i 1 10
;     (display i))

(lambda ()
  (define (loop i n)
    (if (or (< i n) (= i n))
	(begin (display i)
	       (loop (+ 1 i) n))))
  (loop 1 10))

(lambda ()
  (define (loop i n)
    (if (or (< i n) (= i n))
	(begin
	  (display i)
	  (loop (+ 1 i) n)))))

(define (for? exp) (tagged-list? exp 'for))
(define (for-var exp) (cadr exp))
(define (for-init exp) (caddr exp))
(define (for-limit exp) (cadddr exp))
(define (for-body exp) (cddddr exp))

(define (for->combination exp)
  (list
   (make-lambda '()
		(list (list
		       'define
		       (list 'loop (for-var exp) 'n)
		       (list 'if
			     (list 'or
				   (list '< (for-var exp) 'n)
				   (list '= (for-var exp) 'n))
			     (cons 'begin
				   (append (for-body exp)
					   (list (list 'loop
						       (list '+ 1 (for-var exp))
								 'n))))))
		      (list 'loop (for-init exp) (for-limit exp))))))

