;; Chapter 5, Control

;; 1. Translate the following expressions into equivalent expressions that don't
;; use let or let* and don't cause an expression to be evaluated twice.

;(let ((x (car y)))
;  (cons x x))
((lambda (x)
  (cons x x))
 (car y))

;(let* ((w (car x))
;       (y (+ w z)))
;  (cons w y))
((lambda (w)
   ((lambda (y)
      (cons w y))
    (+ w z)))
 (car x))

;; 2. Re-write mystery to use cond
(defun mystery (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
	  0
	  (let ((z (mystery x (cdr y))))
	    (and z (+ z 1))))))

(defun mystery-cond (x y)
  (cond ((null y) nil)
	((eql (car y) x) 0)
	(t (let ((z (mystery-cond x (cdr y))))
	     (and z (+ z 1))))))

;; 3.
(defun sq-if-not-less-5 (x)
  (if (and (> x 0) (< x 5) (= x 5))
      nil
      (* x x)))

;; 5. 
(defun precedes-r (obj v)
  (let ((pos (position obj v)))
    (cond (pos
	   (if (> pos 0)
	       (adjoin (elt v (- pos 1))
		     (precedes-r obj (subseq v (1+ pos))))
	       (precedes-r obj (subseq v (1+ pos)))))
	  (t nil))))

(defun precedes-iter (x v)
  (let ((list nil))
	(do ((pos (position x v)
		  (position x v :start (1+ pos))))
	    ((null pos) nil)
	  (when (and (> pos 0)
		     (not (member (elt v (- pos 1)) list)))
	    (push (elt v (- pos 1)) list)))
	list))

;; 6.
(defun intersperse-r (obj lst)
  (cond ((null (cdr lst)) lst)
	(t (cons (car lst)
		 (cons obj
		       (intersperse-r obj (cdr lst)))))))

(defun intersperse-iter (obj lst)
  (do ((acc nil)
       (d (cdr lst) (cdr d))
       (a (car lst) (car d)))
      ((null d) 
       (reverse (cons a acc)))
    (setf acc (cons obj
		    (cons a acc)))))

;; 7. Define a function which takes a list of numbers and returns true iff the
;; difference of successive pairs of them is 1, using:

; a) recursion
(defun diffsp1-r (lst)
  (if (null (cdr lst))
      t
      (and (= 1 (abs (- (car lst) (cadr lst))))
	   (diffsp1-r (cdr lst)))))

; b) do
(defun diffsp1-d (lst)
  (do ((l lst (cdr l)))
      ((or (null (cdr l))
	   (not (= 1
		   (abs (- (car l) (cadr l))))))
       (null (cdr l)))))

; c) mapc and return

(defun diffsp1-m (lst)
  (mapc #'(lambda (a d)
	    (when (not (= 1
			  (abs (- a d))))
	      (return-from diffsp1-m nil)))
	lst
	(cdr lst))
  t)

;; 8. Define a single recursive function that returns, as two values, 
;; the minimum and maximum elements of a vector

(defun maxmin (v)
  (let ((len (- (length v) 1)))
    (maxmin-r v (elt v len) (elt v len) (- len 1))))

(defun maxmin-r (v mx mn pos)
  (if (< pos 0)
      (values mx mn)
      (let ((val (elt v pos)))
	(maxmin-r v
		  (if (< mx val) mx val)
		  (if (> mn val) mn val)
		  (- pos 1)))))