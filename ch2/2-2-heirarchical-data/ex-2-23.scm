(define for-each
  (lambda (f l)
    (cond ((null? l) #t)
          (else (f (car l)) ; This is sort of weird, I think it's the first
                            ; time I've used this more imperative style
                (for-each f (cdr l)))))) ;
