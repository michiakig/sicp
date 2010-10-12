;;;; Ex. 3.25 n-dimensional tables

;;; use assoc from the text:
(define (assoc key records)
  (cond ((null? records) #f)
	((equal? key (caar records)) (car records))
	(else (assoc key (cdr records)))))

;;; The basic idea is that in order to be able to store values at arbitrary depths,
;;; for each key at a given depth there is a value and a table. We store this as a
;;; pair, with an empty table if there is nothing "deeper" under that key, and null
;;; if there is no value stored at that depth under that key.

;;; We store tables at every level as (*table* ((key . value) ...))

;;; Lookup and insert functions recurse over the list of keys given and down the 
;;; hierarchy of tables

;;; undefined if keys is null - what's the point of (lookup '() a-table)
(define (lookup keys table)
  (let ((record (assoc (car keys) (cdr table))))
    (cond ((last-key? keys)
	   (if record
	       (get-value-at record)
	       #f))
	  (record
	   (lookup (cdr keys) (get-table-at record)))
	  (else #f))))

(define (insert! keys value table)
  (let ((record (assoc (car keys) (cdr table))))
    (display keys)
    (display value)
    (display table)
    (cond (record ; (car key) exists on this level
	   (cond ((last-key? keys)
		  (set-value-at record value))
		 (else (insert! (cdr keys) value (get-table-at record))))) ; recur
	  (else
	   (cond ((last-key? keys)
		  (set-cdr! table (cons (new-record (car keys)
						    value
						    (make-table))
					(cdr table))))
		 (else
		  (set-cdr! table (cons (new-record (car keys)
						    '()
						    (make-table))
					(cdr table)))
		  (insert! (cdr keys) value (get-table-at (cadr table)))))))))

(define (last-key? keys)
  (null? (cdr keys)))

(define (make-table)
  (list '*table*))

(define (get-value-at record) (cadr record))
(define (get-table-at record) (cddr record))
(define (set-value-at record value)
  (set-cdr! record (cons value (get-table-at record))))
(define (set-table-at record table)
  (set-cdr! (cdr record) table))
(define (new-record key value table)
  (cons key (cons value table)))