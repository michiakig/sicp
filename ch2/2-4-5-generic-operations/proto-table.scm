;;;; Prototype table

;;; In section 2.4.3 the authors introduce "data-directed programming," and 
;;; describe a table of operations for the complex-number system. But they 
;;; put off implementing the table itself until section 3.3.3, which means 
;;; it's hard to try out any of the exercises yet. So here is a simple 
;;; alist-based table to tide me over until Chapter 3.

;; Two tables, one for operators and one for coercion functions
(define op-table '())
(define coercion-table '())

;; Public interface to the tables (used by apply-generic)
(define (get op type)
  (get-table op type op-table))
(define (put op type item)
  (set! op-table (put-table op type item op-table)))

(define (get-coercion type1 type2)
  (get-table type1 type2 coercion-table))
(define (put-coercion type1 type2 fn)
  (set! coercion-table (put-table type1 type2 fn coercion-table)))

;; All the functions below are interior to the table
(define (get-table op type table)
  (let ((row (lookup op table)))
    (if row
        (lookup type row)
        #f)))

(define (put-table op type item table)
  (let ((row (lookup op table)))
    (let ((new-row (install type item (if row row '()))))
      (install op new-row table))))

;; actually could have just used assoc
(define (lookup key alist)
  (cond ((null? alist) #f)
        ((equal? key (caar alist)) (cdar alist))
        (else (lookup key (cdr alist)))))

(define (install key value alist)
  (cond ((null? alist) (cons (cons key value) '()))
        ((equal? key (caar alist))
         (cons (cons key value) (cdr alist)))
        (else (cons (car alist)
                    (install key value (cdr alist))))))
