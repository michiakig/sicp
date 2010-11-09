;;;; Ex. 3.59 Power series as streams

;; My memory of power series is very faint at best, so this could all be 
;; quite wrong ... 

;; a)
(defun integrate-series (s)
  (stream-map #'* s (stream-map #'(lambda (x) (/ 1 x)) integers)))

;; b)
(setf exp-series (cons-stream 1 (integrate-series exp-series)))

(setf cosine-series
      (cons-stream 1 (scale-stream
		      (integrate-series sine-series) -1)))))

(setf sine-series (cons-stream 0 (integrate-series consine-series)))

;;; Ex. 3.60

(defun mul-series (s1 s2)
  (cons-stream (* (stream-car s1)
		  (stream-car s2))
	       (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
			    (mul-streams (stream-cdr s1) s2))))

;;; Ex. 3.61

(defun invert-unit-series (s)
  (cons-stream 1 (scale-stream (mul-series (stream-cdr s)
					   (invert-unit-series s)) -1)))

;;; Ex. 3.62

(defun div-series (num den)
  (let ((d (/ 1 (stream-car den))))
    (scale-stream d
		  (mul-series num
				(invert-unit-series (scale-stream den d))))))
			   
  