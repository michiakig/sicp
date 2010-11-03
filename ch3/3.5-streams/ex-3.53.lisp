
;;; Ex. 3.53
;; Describe the elements of the stream defined by

(setf s (cons-stream 1 (add-streams s s)))

;; this is just like double at the top of the page (p 330)

;;; Ex. 3.54
;; Define mul-streams
;; Complete the definition of the stream whose nth element 
;; (counting from 0) is n+1 factorial

(defun mul-streams (s1 s2)
  (stream-map #'* s1 s2))

(setf factorials (cons-stream 1
			      (mul-streams (stream-cdr integers)
					   factorials)))
;; Ex. 3.55
;; down the rabbit hole...

(defun partial-sums (s)
  (cons-stream (stream-car s)
	       (add-streams (stream-cdr s)
			    (partial-sums s))))

