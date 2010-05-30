; (not a programming exercise ...)

; Observe that our model of evaluation allows for combinations whose operators are compound expressions. Use this observation to describe the behavior of the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; Answer: If b is greater than zero, than the result is the sum of a and b.
; But if b is not greater than zero, the result is the difference of a and b.
; Essentially, as the function is named, it adds the absolute value of b to a.
