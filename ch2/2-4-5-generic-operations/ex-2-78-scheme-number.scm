;;;; Exercise 2.78
;;; Modify the definitions of type-tag, contents, and attach-tag from
;;; section 2.4.2 so that our generic system takes advantage of Scheme's internal
;;; type system. 

;;; This file needs to be loaded after simple-apply-generic.scm (or after the
;;; call to init) in order to work, overriding the definitions in that file.

;;; I was suprised how little effort this took to work, I suppose it is a testament
;;; to the flexibility of the system

(define (attach-tag type-tag contents)
  (cond ((number? contents) contents)
        (else (cons type-tag contents))))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        (else (car datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        (else (cadr datum))))
