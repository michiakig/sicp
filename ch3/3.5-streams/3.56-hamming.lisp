
;;; Ex 3.56
;; This stream has the property that all elements have no prime factors
;; other than 2, 3, or 5.

(setf S (cons-stream 1 (merge-streams (scale-stream S 2)
			      (merge-streams (scale-stream S 3)
				     (scale-stream S 5)))))