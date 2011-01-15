;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 5 Section 1 Designing Register Machines

;;;; Exercise 5.2

(data-paths
 (registers
  ((name p)
   (buttons ((name p<-p*c) (source (operation *)))))
  ((name c)
   (buttons ((name c<-c+1) (source (operation 1+)))))
  ((name n))))

(operations
 ((name *)
  (inputs (register p) (register c)))
 ((name >)
  (inputs (register c) (register n))))

(controller
 test-c
 (test >)
 (branch (label fact-done))
 (p<-p)
 (c<-c+1)
 (goto (label test-c))
 fact-done)
