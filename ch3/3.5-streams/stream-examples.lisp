
;; Stream of integers

(defun integers-starting-from-n (n)
  (cons-stream n (integers-starting-from-n (+ n 1))))

(setf integers (integers-starting-from-n 1))

