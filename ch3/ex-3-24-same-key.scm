;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 3 Section 3 Modeling with Mutable Data

;;;; Exercise 3.24

;;; make-table now takes as an argument same-key? which is used to test equality 
;;; of keys in assoc, which now also takes an extra equality predicate argument.

(define (assoc key records test?) ; ***
  (cond ((null? records) false)
        ((test? key (caar records)) (car records)) ;***
        (else (assoc key (cdr records) test?))))

(define (make-table same-key?) ;***
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table) same-key?))) ;***
        (if subtable
            (let ((record (assoc key-2 (cdr subtable) same-key?))) ;***
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table) same-key?))) ;***
        (if subtable
            (let ((record (assoc key-2 (cdr subtable) same-key?))) ;***
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))