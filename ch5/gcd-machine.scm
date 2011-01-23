;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 5 Section 1 Designing Register Machines

(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))

(begin 
  (set-register-contents! gcd-machine 'a 206)
  (set-register-contents! gcd-machine 'b 40)
  (start gcd-machine)
  (get-register-contents gcd-machine 'a))


