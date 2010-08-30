;;;; This is the simple version of apply-generic (does not do any coercion)
;;;; which appears in the text on page 184

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

;;;; Exercise 2.78
;;; Modify the definitions of type-tag, contents, and attach-tag from
;;; section 2.4.2 so that our generic system takes advantage of Scheme's 
;;; internal type system. 

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
