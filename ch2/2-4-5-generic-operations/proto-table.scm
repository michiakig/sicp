;;;; Prototype table

;;; In section 2.4.3 the authors introduce "data-directed programming," and 
;;; describe a table of operations for the complex-number system. But they 
;;; put off implementing the table itself until section 3.3.3, which means 
;;; it's hard to try out any of the exercises yet. So here is a simple 
;;; alist-based table to tide me over until Chapter 3.

(define table '())

(define (get op type)
  (let ((row (lookup op table)))
    (if row
        (lookup type row)
        #f)))

(define (put op type item)
  (let ((row (lookup op table)))
    (let ((new-row (install type item (if row row '()))))
      ;; It's worth noting that assignment (set!) hasn't been introduced yet
      ;; Which is probably why the authors chose to save these for Chapter 3
      (set! table (install op new-row table)))))

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


