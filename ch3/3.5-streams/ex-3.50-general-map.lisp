;;; Ex 3.50
;;; complete the implementation of a generalized stream-map to allow procedures 
;;; which take multiple arguments, analogous to map in section 2.2.3 footnote 12.

(defun stream-map (proc &rest args)
  (if (stream-null? (car args))
      the-empty-stream
      (cons-stream
       (apply proc (mapcar #'stream-car args))
       (apply #'stream-map (cons proc (mapcar #'stream-cdr args))))))
