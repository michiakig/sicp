;; Ex. 3.63
;; Using the local variable is more efficient because each element of the 
;; stream will only be computed once, due to the memo-proc optimization/
;; The crucial piece here is that in Louis Reasoner's the recursive call 
;; to sqrt-stream is a function call, and therefore each call will essentially
;; start over from scratch, whereas the recursive reference to the local 
;; variable refers to the same stream each time, which has cached the previous
;; results for each element in a memo-proc closure.

;; Ex. 3.64

;; another utility - analogous to maplist 

(defun stream-maplist (proc s)
  (if (stream-null? s)
      the-empty-stream
    (cons-stream (funcall proc s)
		 (stream-maplist proc (stream-cdr s)))))

(defun stream-limit (s tolerance)
  (labels ((proc (s)
		 (if (< (abs (- (stream-car s) (stream-car (stream-cdr s))))
			tolerance)
		     (stream-car (stream-cdr s))
		   (proc (stream-cdr s)))))
	  (proc s)))

;; Ex. 3.64

(defun ln-summands (n)
  (cons-stream (/ 1.0 n)
	       (stream-map #'- (ln-summands (1+ n)))))

(setf ln-stream (partial-sums (ln-summands 1)))

(setf ln-stream2 (euler-transform ln-stream))

(setf ln-stream3 (accelerated-sequence #'euler-transform ln-stream))
