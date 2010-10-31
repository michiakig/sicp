
;;; Ex 3.50
;;; complete the implementation of a generalized stream-map to allow procedures 
;;; which take multiple arguments, analogous to map in section 2.2.3 footnote 12.

(defun stream-map (proc &rest args)
  (if (stream-null? (car args))
      the-empty-stream
      (cons-stream
       (apply proc (mapcar #'stream-car args))
       (apply #'stream-map (cons proc (mapcar #'stream-cdr args))))))

;;; Ex. 3.51

(defun show (x)
  (display-line x)
  x)

;; what does the interpreter print after 

(setf x (stream-map #'show (stream-enumerate-interval 0 10)))

(stream-ref x 5)
;; 1
;; 2
;; 3
;; 4
;; 5

(stream-ref x 7)
;; 6
;; 7

;;; Ex. 3.52

(setf sum 0)
;; sum is 0

(defun accum (x)
  (setf sum (+ x sum))
  sum)
;; sum is 0

(setf seq (stream-map #'accum (stream-enumerate-interval 1 20)))
;; sum is 1

(setf y (stream-filter #'evenp seq))
;; sum is 6

(setf z (stream-filter #'(lambda (x) (= (rem x 5) 0)) seq))
;; sum is 10

(stream-ref y 7)
;; sum is 136

(display-stream z)
;; 10
;; 15
;; 45
;; 55
;; 105
;; 120
;; 190
;; 210

;; sum is 210

;; Yes, the results above would be different if delay were not optimized with 
;; memo-proc, since accum would be evaluated more than once for each input,
;; once when (stream-ref x 7) was evaluated, and again when (display-stream z) was
;; evaluated.

