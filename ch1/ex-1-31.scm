; Recursive product function
(define product
  (lambda (term a next b)
    (if (> a b)
        1
        (* (term a)
           (product term (next a) next b)))))

; Iterative one
(define product
  (lambda (term a next b)
    (define iter
      (lambda (a result)
        (if (> a b)
            result
            (iter (next a) (* result (term a))))))
    (iter a 1)))
