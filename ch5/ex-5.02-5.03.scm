;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 5 Section 1 Designing Register Machines

;;;; Exercise 5.2

'(data-paths
 (registers
  ((name p)
   (buttons ((name p<-p*c) (source (operation *)))))
  ((name c)
   (buttons ((name c<-c+1) (source (operation 1+)))))
  ((name n))))

'(operations
 ((name *)
  (inputs (register p) (register c)))
 ((name >)
  (inputs (register c) (register n))))

'(controller
 test-c
 (test (op >))
 (branch (label fact-done))
 (p<-p)
 (c<-c+1)
 (goto (label test-c))
 fact-done)

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

(set-register-contents! fact-machine 'p 1)
(set-register-contents! fact-machine 'c 1)
(set-register-contents! fact-machine 'n 5)
(start fact-machine)
(get-register-contents fact-machine 'p)

;;;; Exercise 5.3

'(controller
 test-g
 (test (op good-enough?))
 (branch (label sqrt-done))
 (g<-i)
 (goto (label test-g))
 (sqrt-done))

