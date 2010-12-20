;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 2 Section 4 Multiple Representations for Abstract Data
;;;; Chapter 2 Section 5 Systems with Generic Operations

;;; Exercise 2.75

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          (else
            (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)
