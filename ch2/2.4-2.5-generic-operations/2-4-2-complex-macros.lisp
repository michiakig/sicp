; 2.4.2 Tagged data

; I started reading Peter Seibel's Practical Common Lisp this 
; week, which gets right in to describing Lisp macros in the 3rd 
; chapter.

; This is a quick attempt at writing my first Lisp macro.
; After seeing the repetitive code for the generic selectors
; in this section on data tagged with type information, I 
; thought it might be good to try to write a macro for it.



(defun square (x)
  (* x x))

(defun add-complex (z1 z2)
  (make-from-real-imag-rectangular (+ (real-part z1) (real-part z2))
		       (+ (imag-part z1) (imag-part z2))))
(defun sub-complex (z1 z2)
  (make-from-real-imag-rectangular (- (real-part z1) (real-part z2))
		       (- (imag-part z1) (imag-part z2))))
(defun mul-complex (z1 z2)
  (make-from-mag-ang-rectangular (* (magnitude z1) (magnitude z2))
		     (+ (angle z1) (angle z2))))
(defun div-complex (z1 z2)
  (make-from-mag-ang-rectangular (/ (magnitude z1) (magnitude z2))
		     (- (angle z1) (angle z2))))

; Ben's representation, internally as real and imaginary parts
(defun real-part-rectangular (z) (car z))
(defun imag-part-rectangular (z) (cdr z))
(defun magnitude-rectangular (z)
  (sqrt (+ (square (real-part-rectangular z))
	   (square (imag-part-rectangular z)))))
(defun angle-rectangular (z)
  (atan (imag-part-rectangular z) (real-part-rectangular z)))
(defun make-from-real-imag-rectangular (x y)
  (attach-tag 'rectangular (cons x y)))
(defun make-from-mag-ang-rectangular (r a)
  (attach-tag 'rectangular (cons (* r (cos a)) (* r (sin a)))))

; Alyssa's representation, internally as angle and magnitude
(defun real-part-polar (z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(defun imag-part-polar (z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(defun magnitude-polar (z) (car z))
(defun angle-polar (z) (cdr z))
(defun make-from-real-imag-polar (x y)
  (attach-tag 'polar
	      (cons (sqrt (+ (square x) (square y)))
		    (atan y x))))
(defun make-from-mag-ang-polar (r a)
  (attach-tag 'polar (cons r a)))

(defun attach-tag (type-tag contents)
  (cons type-tag contents))
(defun type-tag (datum)
  (car datum))
(defun contents (datum)
      (cdr datum))

(defun rectangular? (z)
  (eql (type-tag z) 'rectangular))
(defun polar? (z)
  (eql (type-tag z) 'polar))

(defmacro mk-complex-selector (name rsel psel)
  `(defun ,name (z)
     (cond ((rectangular? z)
	    (,rsel (contents z)))
	   ((polar? z)
	    (,psel (contents z)))
	   (t (error "Unknown type")))))

(mk-complex-selector real-part real-part-rectangular real-part-polar)
(mk-complex-selector imag-part imag-part-rectangular imag-part-polar)
(mk-complex-selector magnitude magnitude-rectangular magnitude-polar)
(mk-complex-selector angle angle-rectangular angle-polar)
