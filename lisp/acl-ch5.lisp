(defun precedes-iter (x v)
  (let ((list nil))
	(do ((pos (position x v)
		  (position x v :start (1+ pos))))
	    ((null pos) nil)
	  (when (and (> pos 0)
		     (not (member (elt v (- pos 1)) list)))
	    (push (elt v (- pos 1)) list)))
	list))

(defun intersperse-r (obj list)
  (cond ((null list) nil)
	((null (cdr list)) list)
	(t (cons (car list)
		 (cons obj
		       (intersperse-r obj (cdr list)))))))

