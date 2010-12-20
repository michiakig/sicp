;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 2 Section 3 Symbolic Data

;;; Exercise 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (encode-iter symbol tree result)
    (cond ((leaf? tree) result)
          ((element-of-set? symbol
                            (symbols (left-branch tree))) ; safe b/c specifically designed to return a set
                                                          ; in the case that (left-branch tree) is a leaf
           (encode-iter symbol
                        (left-branch tree)
                        (append result (list 0))))  ; append an element to a list, O(n) of course
          ((element-of-set? symbol
                            (symbols (right-branch tree)))
           (encode-iter symbol
                        (right-branch tree)
                        (append result (list 1))))
          (else (error "symbol not in tree!"))))
  (encode-iter symbol tree '()))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
