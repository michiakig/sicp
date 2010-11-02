;;;; Stream utility functions from Chapter 3. Requires the stream implementation
;;; code from the text, pp 319-322

(defun stream-ref (s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(defun stream-map (proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (funcall proc (stream-car s))
		   (stream-map proc (stream-cdr s)))))

(defun stream-for-each (proc s)
  (if (stream-null? s)
      'done
      (progn (funcall proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(defun display-stream (s)
  (stream-for-each #'display-line s))

(defun display-line (x)
  (format t "~%~A" x))

(defun stream-enumerate-interval (low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(defun stream-filter (pred s)
  (cond ((stream-null? s) 
	 the-empty-stream)
	((funcall pred (stream-car s))
	 (cons-stream (stream-car s)
		      (stream-filter pred (stream-cdr s))))
	(t (stream-filter pred (stream-cdr s)))))

;; Additional code from the text, pp 326-

(defun add-streams (s1 s2)
  (stream-map #'+ s1 s2))

;; merge procedure from exercise 3.56

(defun merge (s1 s2)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(t
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((< s1car s2car)
		  (cons-stream s1car (merge (stream-cdr s1) s2)))
		 ((> s1car s2car)
		  (cons-stream s2car (merge s1 (stream-cdr s2))))
		 (t
		  (cons-stream s1car
			       (merge (stream-cdr s1)
				      (stream-cdr s2)))))))))