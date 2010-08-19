(defun new-union (l1 l2)
  (let ((acc nil))
    (dolist (x l1)
      (if (member x l2)
	  (progn
	    (setf l2 (remove x l2))
	    (setf acc (append acc (list x))))
	  (setf acc (append acc (list x)))))
    (setf acc (append acc l2))
    acc))

(defun occurences (lst)
  (let ((alist nil))
    (do ((tail (cdr lst) (cdr tail))
	 (head (car lst) (car tail)))
	((null tail) (sort alist
			   #'(lambda (x y) (> (cdr x) (cdr y)))))
      (let ((val (assoc head alist)))
	(if (null val)
	    (setf alist (cons (cons head 1) alist))
	    (incf (cdr val)))))))

(defun pos+iter (lst)
  (let ((acc nil))
    (do ((i 0 (1+ i))
	 (tail (cdr lst) (cdr tail))
	 (head (car lst) (car tail)))
	((null head) acc)
      (setf acc (append acc (list (+ i head)))))))

(defun pos+r (lst)
  (defun pos+r-help (lst i)
    (if (null lst)
	nil
	(cons (+ i (car lst))
	      (pos+r-help (cdr lst) (1+ i)))))
  (pos+r-help lst 0))

(defun pos+mapcar (lst)
  (let ((i 0))
    (mapcar #'(lambda (x)
		(incf i)
		(- (+ x i) 1))
	    lst)))

(defun gcons (a d)
  (cons d a))
(defun glist (&rest rest)
  (if (null rest)
      nil
      (if (null (cdr rest))
	  (gcons (car rest) nil)
	  (gcons (car rest) (apply #'glist (cdr rest))))))
(defun glength (lst)
  (if (null lst)
      0
      (1+ (glength (car lst)))))
(defun gmember (item lst)
  (if (null lst)
      nil
      (if (eql (cdr lst) item)
	  t
	  (gmember item (car lst)))))