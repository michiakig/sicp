;;;; Common Lisp implementation of SICP streams from chapter 3, section 5

(defparameter the-empty-stream nil)
(defun stream-null? (x) (null x))

;;; delay and force are the primitives which support controlling execution
;;; delay must be a "special form" or a macro
(defmacro delay (x)
  `(memo-proc (lambda () ,x)))
(defun force (x) (funcall x))

(defun memo-proc (proc)
  (let ((already-run? nil)
	(result nil))
    (lambda ()
      (if (not already-run?)
	  (progn (setf result (funcall proc))
		 (setf already-run? t)
		 result)
	  result))))

(defun stream-car (s) (car s))
(defun stream-cdr (s) (force (cdr s)))
;;; cons-stream must also be a macro to control the execution of y
(defmacro cons-stream (x y)
  `(cons ,x (delay ,y)))

(defun stream-ref (s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;;; Ex 3.50
;;; complete the implementation of a generalized stream-map to allow procedures 
;;; which take multiple arguments, analogous to map in section 2.2.3 footnote 12.
(defun stream-map (proc &rest args)
  (if (stream-null? (car args))
      the-empty-stream
      (cons-stream
       (apply proc (mapcar #'stream-car args))
       (apply #'stream-map (cons proc (mapcar #'stream-cdr args))))))

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

;;; Ex. 3.51

(defun show (x)
  (display-line x)
  x)

(defun stream-filter (pred s)
  (cond ((stream-null? s) 
	 the-empty-stream)
	((funcall pred (stream-car s))
	 (cons-stream (stream-car s)
		      (stream-filter pred (stream-cdr s))))
	(t (stream-filter pred (stream-cdr s)))))

