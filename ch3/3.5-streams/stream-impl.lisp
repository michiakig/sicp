;;;; Common Lisp implementation of streams from chapter 3, section 5
;;; (I'm more comfortable with Common Lisp macros than Scheme macros.)

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
