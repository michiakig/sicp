(defun elt-list? (x)
  (if (null x)
      nil
      (if (listp (car x))
	  t
	  (elt-list? (cdr x)))))

(defun print-dots-iter (n)
  (do ((i 0 (+ i 1)))
      ((= i n) 'done)
    (format t ".")))

(defun print-dots-r (n)
  (if (= n 0)
      'done
      (progn
	(format t ".")
	(print-dots-r (- n 1)))))

(defun count-member-iter (sym lst)
  (let ((n 0))
    (dolist (x lst)
      (if (eql sym x)
	  (incf n 1)))
    n))

(defun count-member-r (sym lst)
  (if (null lst)
      0
      (if (eql sym (car lst))
	  (+ 1 (count-member-r sym (cdr lst)))
	  (count-member-r sym (cdr lst)))))

; remove is not destructive, so the first call does nothing to lst
; need to nest the call to remove inside the call to apply
(defun summit-1 (lst)
  (apply #'+ (remove nil lst)))

; there is no check for the base case
; needs to check for when lst is null or it will loop forever
(defun summit-2 (lst)
  (if (null lst)
      0
      (let ((x (car lst)))
	(if (null x)
	    (summit-2 (cdr lst))
	    (+ x (summit-2 (cdr lst)))))))
      