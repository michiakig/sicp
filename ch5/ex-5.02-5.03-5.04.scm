;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 5 Section 1 Designing Register Machines

;;;; Exercise 5.2

;; actual register machine definition and driving code

(define fact-machine
  (make-machine
   '(p c n)
   (list (list '* *) (list '> >) (list '+ +))
   '(test-c
     (test (op >) (reg c) (reg n))
     (branch (label fact-done))
     (assign p (op *) (reg p) (reg c))
     (assign c (op +) (reg c) (const 1))
     (goto (label test-c))
     fact-done)))

(begin 
  (set-register-contents! fact-machine 'p 1)
  (set-register-contents! fact-machine 'c 1)
  (set-register-contents! fact-machine 'n 5)
  (start fact-machine)
  (get-register-contents fact-machine 'p))


;;;; Exercise 5.3

;; actual register machine definition and driving code

(define sqrt-machine
  (make-machine
   '(x g t)
   (list (list 'square square)
         (list '- -)
         (list 'abs abs)
         (list '< <)
         (list '/ /)
         (list 'average (lambda (a b) (/ (+ a b) 2))))
   '(test-g
     (assign t (op square) (reg g))
     (assign t (op -) (reg t) (reg x))
     (assign t (op abs) (reg t))
     (test (op <) (reg t) (const 0.001))
     (branch (label sqrt-done))
     (assign t (op /) (reg x) (reg g))
     (assign g (op average) (reg g) (reg t))
     (goto (label test-g))
     sqrt-done)))

(begin 
  (set-register-contents! sqrt-machine 'g 1.0)
  (set-register-contents! sqrt-machine 'x 16)
  (start sqrt-machine)
  (get-register-contents sqrt-machine 'g))

;;;; Exercise 5.4
;;;; Exercise 5.7

;; a. recursive exponentiation
(define expt-machine
  (make-machine
   '(b n val continue)
   (list (list '= =) (list '* *) (list '- -))
   '((assign continue (label expt-done))
     expt-loop
     (test (op =) (reg n) (const 0))
     (branch (label base-case))
     (save continue)
     (save b)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-expt))
     (goto (label expt-loop))
     after-expt
     (restore b)
     (restore continue)
     (assign val (op *) (reg b) (reg val))
     (goto (reg continue))     
     base-case
     (assign val (const 1))
     (goto (reg continue))
     expt-done)))

(begin 
  (set-register-contents! expt-machine 'b 2)
  (set-register-contents! expt-machine 'n 3)
  (start expt-machine)
  (get-register-contents expt-machine 'val))

;; b. iterative exponentiation

(define iter-expt-machine
  (make-machine
   '(b n c p)
   (list (list '= =) (list '- -) (list '* *))
   '((assign c (reg n))
     (assign p (const 1))
     test-c
     (test (op =) (reg c) (const 0))
     (branch (label done))
     (assign c (op -) (reg c) (const 1))
     (assign p (op *) (reg b) (reg p))
     (goto (label test-c))
     done)))

(begin 
  (set-register-contents! iter-expt-machine 'b 3)
  (set-register-contents! iter-expt-machine 'n 4)
  (start iter-expt-machine)
  (get-register-contents iter-expt-machine 'p))
