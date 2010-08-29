;;;; Chapter 6, Functions

;;; 1. Define a version of tokens that takes :test and :start arguments defaulting
;;; to #'constituent and 0 respectively.

(defun tokens (str &key (test #'constituent) (start 0))
  (let ((p1 (position-if test str :start start)))
    (if p1
	(let ((p2 (position-if #'(lambda (c)
				   (not (funcall test c)))
			       str :start p1)))
	  (cons (subseq str p1 p2)
		(if p2
		    (tokens str :test test :start p2)
		    nil)))
	nil)))

(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\space ))))

;;; 2. Define a version of bin-search that takes :key, :test, :start, and :end 
;;; arguments passed to it.

(defun bin-search (obj vec &key (test #'eql) (start 0) (end (1- (length vec))))
  (let ((range (- end start)))
    (if (zerop range)
	(if (funcall test obj (aref vec start))
	    obj
	    nil)
	(let* ((mid (+ start (round (/ range 2))))
	       (obj2 (aref vec mid)))
	  (if (< obj obj2)
	      (bin-search obj vec :test test :start start :end (- mid 1))
	      (if (> obj obj2)
		  (bin-search obj vec :test test :start (+ mid 1) :end end)
		  obj))))))

;;; 3. Define a function that takes any number of args and returns the number of
;;; args passed to it

(defun count-args (&rest rest)
  (length rest))

;;; 4. Modify most to return, as two values, the two highest scoring elements

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((frst (car lst))
	     (frstscore (funcall fn frst))
	     (scnd nil)
	     (scndscore nil))
	(dolist (obj (cdr lst))
	  (let ((score (funcall fn obj)))
	    (when (> score frstscore)
	      (setf scnd frst
		    scndscore frstscore
		    frst obj
		    frstscore score))))
	(values (list frst frstscore)
		(list scnd scndscore)))))

;;; 5. Define remove-if in terms of filter

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push val acc))))
    (nreverse acc)))

(defun my-remove-if (fn lst)
  (filter #'(lambda (x)
	      (if (not (funcall fn x))
		  x
		  nil))
	  lst))

;;; 6. Define a function which takes a number and returns the greatest number
;;; it has seen so far.

(let ((greatest nil))
  (defun greatest-so-far (x)
    (if (and greatest (> greatest x))
	greatest
	(setf greatest x))))

;;; 7. Define a function that takes one argument, a number, and returns true if it
;;; is greater than the argument the last time the function was called.  Returns nil
;;; the first time.

(let ((last nil))
  (defun greater-than-last-time (x)
    (let ((retval (and last (> x last))))
      (setf last x)
      retval)))

;;; 8. Supposed expensive is a function of x, a number in [0,100] that returns the 
;;; result of of a time consuming computation.  Define frugal which returns the same 
;;; answer but only calls expensive when given an argument it has not seen before.

;; for our purposes, use sqrt
(defun expensive (x)
  (sqrt x))

;; actually this will work for all numbers, not just [1,100]
(let ((ht (make-hash-table)))
  (defun frugal (x)
    (let ((val (gethash x ht)))
      (if val
	  val
	  (setf (gethash x ht)
		(expensive x))))))

;;; 9. Define a function like apply, but where any number printed out before it 
;;; returns will be printed in octal.

(defun octal-apply (fn args)
  (let ((*print-base* 8))
    (apply fn args)))