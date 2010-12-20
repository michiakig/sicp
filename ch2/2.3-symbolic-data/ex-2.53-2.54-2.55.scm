;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 2 Section 3 Symbolic Data

;;; Exercise 2.53

;; (list 'a 'b 'c)
;; > (a b c)
;; (list (list 'george))
;; > ((george))
;; (cdr '((x1 x2) (y1 y2)))
;; > ((y1 y2))
;; (cadr '((x1 x2) (y1 y2)))
;; > (y1 y2)
;; (pair? (car '(a short list)))
;; > #f
;; (memq 'red '((red shoes) (blue socks)))
;; > #f
;; (memq 'red '(red shoes blue socks))
;; > (red shoes blue socks)

;; defined in the text:
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;;; Exercise 2.54

(define (equal?? l1 l2)
  (cond ((null? l1) (null? l2))
        ((null? l2) #f)
        ((pair? (car l1))
         (and (pair? (car l2))
              (and (equal?? (car l1) (car l2)) (equal?? (cdr l1) (cdr l2)))))
        ((pair? (car l2)) #f)
        (else (and (eq? (car l1) (car l2))
                   (equal?? (cdr l1) (cdr l2))))))
       

;;; Exercise 2.55

;; Eva Lu Ator types to the interpreter the expression (car ''abracadabra).
;; To her surprise, the interpreter prints back quote. Explain.

;; The expression is evaluated as (car (quote (quote abracadabra)))
