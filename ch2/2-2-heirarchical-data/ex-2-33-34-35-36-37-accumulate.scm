; 2.33, basic list-manipulation functions via accumulate
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

; 2.34 Horner's rule for evaluating a polynomial, as an accumulation
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) ; ((...)x + a)
                (+ this-coeff
                   (* x higher-terms)))
              0
              coefficient-sequence))

; 2.35 count-leaves as an accumulation
(define (count-leaves t)
  (accumulate <??> 0 (map <??> <??>)))

; 2.36 accumulate-n
(define (accumulate-n op init seqs)
  (if (null? (car seqs))  ; this is checking (car seqs) and not just seqs
      '()                 ; because the base case is a list like (() () () ...)
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; 2.37 some basic linear algebra 
; dot-product is provided:
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (mi) (dot-product mi v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map <???>
      m)))

; defined for us previously in the text
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
