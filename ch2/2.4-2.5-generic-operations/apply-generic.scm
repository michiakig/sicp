
;; Procedure apply-generic from the text, p196

;; Modified with successive raising for type coercion (ex 2.84)
;; We install a procedure "height" which returns a real representing the height of 
;; the type in the tower. Apply-generic uses this to determine if coercion is needed.
;; This supports extension by allowing a potentially infinite number of types to be 
;; installed between two types in the tower, and as long as all types have raise
;; procedures, it should work, albeit maybe with a lot more work than necessary.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc ; if proc exists at this point, the types are the same
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((height1 (height (car args)))
                    (height2 (height (cadr args)))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (< height1 height2)
                    (apply-generic op (raise a1) a2)
                    (apply-generic op a1 (raise a2))))

;                (let ((t1->t2 (get-coercion type1 type2))
;                      (t2->t1 (get-coercion type2 type1)))
;                  (cond (t1->t2
;                         (apply-generic op (t1->t2 a1) a2))
;                        (t2->t1
;                         (apply-generic op a1 (t2->t1 a2)))
;                        (else
;                         (error "No method for these types -- no coercion available"
;                                (list op type-tags))))))
              (error "No method for these types -- more than 2 args"
                     (list op type-tags)))))))

;; ex 2.78
(define (attach-tag type-tag contents)
  (cond ((number? contents) contents)
        (else (cons type-tag contents))))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "bad tagged datum -- CONTENTS" datum))))
