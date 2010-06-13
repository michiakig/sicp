(define same-parity
  (lambda ints 
    (let ((rem
            (if (even? (car ints)) 0 1)))
      (define same-parity-r
        (lambda (ints)
          (cond ((null? ints) '())
                ((= rem (remainder (car ints) 2))
                 (cons (car ints) (same-parity-r (cdr ints))))
                (else (same-parity-r (cdr ints))))))
      (cons (car ints) (same-parity-r (cdr ints))))))
