(define last-pair
  (lambda (items)
    (cond ((null? (cdr items)) items)
          (else (last-pair (cdr items))))))
