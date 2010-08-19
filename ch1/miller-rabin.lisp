(defun square (x)
  (* x x))

(defun expmod (base exp m)
  (cond ((= exp 0) 1)
	((evenp exp)
	 (mod (square (expmod base (/ exp 2) m)) m)
	(t
	 (mod (* base (expmod base (- exp 1) m))
	      m))))
