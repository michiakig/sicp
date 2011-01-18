;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 5 Section 2 A Register-Machine Simulator



;; a sample machine which operates on a label
(define test-machine
  (make-machine
   '(foo bar baz)
   (list (list '+ +) (list '- -))
   '(start
     (assign foo (op +) (label start) (reg bar)))))

;; I don't know if this is correct - it's not entirely clear what
;; "enforce the condition that operations can be used only with
;; registers and constants" but this new version of
;; make-operation-expression signals an error if one of the operands
;; of an operation is not a constant or register value

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (if (not (or (constant-exp? e) (register-exp? e)))
                    (error "MAKE-OPERATION-EXP -- OPERAND NEITHER CONST NOR REG" e)
                    (make-primitive-exp e machine labels)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))
