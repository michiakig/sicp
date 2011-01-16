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

