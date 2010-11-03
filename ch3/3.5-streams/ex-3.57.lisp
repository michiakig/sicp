
;;; Ex 3.57
;; How many additions are performed when calcuting the nth fibonacci number using
;; the definition of fibs with add-streams?

;; The number of additions is on the order of n (in fact it is n-1).
;; This is because of the memoization optimization which means that for each 
;; element of the stream, there is no need to perform the same additions we 
;; already made for the previous elements, they have been calculated and will
;; be returned from the memo-proc closure.

(setf fibs (cons-stream 0
			(cons-stream 1
				     (add-streams (stream-cdr fibs)
						  fibs))))

;; It's easy to see this by modifying add-streams

(defun add-streams (s1 s2)
  (stream-map #'(lambda (&rest args)
		  (format t "adding~%")
		  (apply #'+ args))
	      s1 s2))

;; And it's easy to see that it would result in exponential additions
;; if we redefine delay as

(defmacro delay (x)
  `(lambda () ,x))