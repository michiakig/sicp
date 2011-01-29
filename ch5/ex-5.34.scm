;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 5 Section 5 Compilation

;;; Exercise 5.34

(pprint (caddr (compile
                '(define (factorial n)
                   (define (iter product counter)
                     (if (> counter n)
                         product
                         (iter (* counter product)
                               (+ counter 1))))
                   (iter 1 1))
                'val
                'next)))

;; Result of compiling iterative factorial

;; Create procedure, jump around body of lambda:
  (assign val (op make-compiled-procedure) (label entry64) (reg env))
  (goto (label after-lambda63))
entry64 ; calls to factorial will enter here
;; Extend environment with args:
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;; Create procedure for internal iter, jump around its body:
  (assign val (op make-compiled-procedure) (label entry69) (reg env))
  (goto (label after-lambda68))
entry69 ; calls to iter will enter here
;; Extend environment with args to iter:
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
;; At this point, save the value of continue, which contains the
;; return point for the outer factorial procedure
  (save continue)
  (save env)
;; Compute (> counter n):
  (assign proc (op lookup-variable-value) (const >) (reg env))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch84))
compiled-branch83
  (assign continue (label after-call82))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch84
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call82 ; val contains result of (> counter n)
  (restore env)
  (restore continue) ; Continue now holds return point for factorial
  (test (op false?) (reg val))
  (branch (label false-branch71))
true-branch72
;; return product
  (assign val (op lookup-variable-value) (const product) (reg env))
  (goto (reg continue)) ; Returns the result of factorial computation
false-branch71
;; In this case, perform the recursive call to iter:
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (save continue) ; Save the return point for factorial
  (save proc)
  (save env)
;; compute (+ counter 1)
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch78))
compiled-branch77
  (assign continue (label after-call76))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch78
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call76 ; val contains result of (+ counter 1)
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
;; compute (* counter product)
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (assign val (op lookup-variable-value) (const product) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch75))
compiled-branch74
  (assign continue (label after-call73))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch75
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call73 ; val contains result of (* counter product)
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc) ; restore iter
  (restore continue) ; Continue now holds the return point for
                     ; factorial, again
;; call proc
  (test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch81))
;; ****
;; This is the essential difference. The goto statement below for the
;; recursive call to iter is the result of the tail-recursive call. In
;; the recursive version of factorial, the continue register holds a
;; label pointing to code which happens after the recursive call -- in
;; this iterative case the continue register holds the return point
;; for the outer factorial procedure, so when the recursive calls
;; finally end, and return, they will return to the caller of
;; factorial directly
compiled-branch80
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
;; ****
primitive-branch81
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call79
after-if70
after-lambda68
;; This is the point in the code right after the internal define of
;; iter. At this point, continue holds whatever the caller of
;; factorial put in that register.
;; Prepare to call (iter 1 1):
  (perform (op define-variable!) (const iter) (reg val) (reg env))
  (assign val (const ok))
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch67))
compiled-branch66
;; Because iter is a compiled procedure, not a primitive, this is the
;; branch that is taken:
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val)) ; goto iter entry point
primitive-branch67
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call65
after-lambda63
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))
