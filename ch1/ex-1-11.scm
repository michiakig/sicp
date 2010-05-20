; SICP exercise 1.11
; A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3. Write a procedure that computes f by means of a recursive process. Write a procedure that computes f by means of an iterative process.

(define f-rec
	(lambda (n)
		(if (< n 3)
				n
				(+	(f-rec (- n 1))
						(* 2 (f-rec (- n 2)))
						(* 3 (f-rec (- n 3)))))))

(define f-iter-start
	(lambda (n)
		(if (< n 3)
				n
				(f-iter n 3 0 1 2))))

(define f-iter
	(lambda (n i a b c)
		(if (= n i)
				(+ c (+ b b) (+ a a a))
				(f-iter n (+ i 1) b c (+ c (+ b b) (+ a a a))))))
