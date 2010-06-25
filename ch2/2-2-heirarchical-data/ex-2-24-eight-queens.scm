; Ex 2.42: "eight queens puzzle" or really n-queens puzzle.

; The function below is defined for us, the exercise is in completing the
; details: safe?, empty-board, adjoin-position, and a representations of a 
; board.
(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; Let's start with safe?, it seems easy.
; Assume that positions is a list of integers, the kth queen is in the
; row (car positions), and the (k-1)th queen is in (car (cdr positions)), etc.
; We only need to check the kth queen, all others are assumed safe.
(define (safe? k positions)
  (define (safe?-r R C pos)
    (cond ((null? pos) #t)
          ((or (= (car pos) R)
               (= (- (car pos) C) R)
               (= (+ (car pos) C) R))
           #f)
          (else (safe?-r R (+ C 1) (cdr pos)))))
  (safe?-r (car positions) 1 (cdr positions)))

; So, based on our assumption above, an empty board is just an empty list:
(define empty-board '())

; Now we also need adjoin-position 
(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

; some basic dependencies:
(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
