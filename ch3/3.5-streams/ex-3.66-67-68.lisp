;;;; streams of infinite pairs, exercises 3.66, 3.67, 3.68 

;; Ex. 3.66


;; Ex. 3.67

;; Ex. 3.68

;; Since this definition doesn't create a new stream with a head and a delayed 
;; tail, it goes into an infinite loop while interleaving the two streams
;; because the second stream is a recursive call to pairs
(defun pairs (s r)
  (interleave
   (stream-map #'(lambda (x) (list (stream-car s) x))
	       r)
   (pairs (stream-cdr s) (stream-cdr r))))


