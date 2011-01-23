;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 5 Section 2

;;;; Exercise 5.14

;; a sort of mini driver to run the factorial machine, print stats, and reset it
(define (fact n)
;; recursive factorial machine from fig. 5.11  
  (define recur-fact-machine
    (make-machine
     '(continue n val)
     (list (list '* *)
           (list '- -)
           (list '= =)
           (list 'display display)
           (list 'read read))
     '((assign continue (label fact-done))
       ;; (perform (op display) (const "\nallocate n:"))
       ;; (assign n (op read))
       fact-loop
       (test (op =) (reg n) (const 1))
       (branch (label base-case))
       (save continue)
       (save n)
       (assign n (op -) (reg n) (const 1))
       (assign continue (label after-fact))
       (goto (label fact-loop))
       after-fact
       (restore n)
       (restore continue)
       (assign val (op *) (reg n) (reg val))
       (goto (reg continue))
       base-case
       (assign val (const 1))
       (goto (reg continue))
       fact-done
       (perform (op print-stack-statistics)))))
  (set-register-contents! recur-fact-machine 'n n)
  (start recur-fact-machine)
  (newline)
  (display (get-register-contents recur-fact-machine 'val))
  ((recur-fact-machine 'stack) 'initialize))

(define (do-fact n)
  (if (= n 0)
      'done
      (begin
        (fact n)
        (do-fact (- n 1)))))


;; From this it's obvious that the stack depth for computing (fact n)
;; is 2n-2

(total-pushes = 18 maximum-depth = 18)
3628800
(total-pushes = 16 maximum-depth = 16)
362880
(total-pushes = 14 maximum-depth = 14)
40320
(total-pushes = 12 maximum-depth = 12)
5040
(total-pushes = 10 maximum-depth = 10)
720
(total-pushes = 8 maximum-depth = 8)
120
(total-pushes = 6 maximum-depth = 6)
24
(total-pushes = 4 maximum-depth = 4)
6
(total-pushes = 2 maximum-depth = 2)
2
(total-pushes = 0 maximum-depth = 0)
1
;Value: done





