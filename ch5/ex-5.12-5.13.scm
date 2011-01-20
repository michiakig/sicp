;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 5 Section 2 A Register Machine Simulator

;;;; Exercise 5.12
;;;; Exercise 5.13

(define (remove-duplicates lst)
  (cond ((null? lst) lst)
        ((member (car lst) (cdr lst))
         (remove-duplicates (cdr lst)))
        (else (cons (car lst)
                    (remove-duplicates (cdr lst))))))

(define (instruction-text-op inst) (car (instruction-text inst)))
(define (instruction-text-reg inst) (cadadr (instruction-text inst)))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations))
        
        (list-insts '())
        (list-entries '())
        (list-saved '())
        (list-sources '()))
    
    (for-each
     (lambda (inst)
       
       (if (not (member (instruction-text inst) list-insts))
           (set! list-insts (cons (instruction-text-op inst) list-insts)))

       (if (and (eq? (instruction-text-op inst)
                     'goto))
           (set! list-entries (cons (instruction-text-reg inst)
                                    list-entries)))

       (if (or (eq? (instruction-text-op inst) 'save)
               (eq? (instruction-text-op inst) 'restore))
           (set! list-saved (cons (cadr (instruction-text inst))
                                  list-saved)))
       
       (if (eq? (instruction-text-op inst)
                'assign)
           (let ((record (assoc (cadr (instruction-text inst)) list-sources)))
             (if record
                 (set-cdr! record (cons (cddr (instruction-text inst))
                                        (cdr record)))
                 (set! list-sources
                       (cons (cons (cadr (instruction-text inst))
                                   (list (cddr (instruction-text inst))))
                             list-sources)))))
       
       (set-instruction-execution-proc! 
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)

    ;; return "extras"
    (list (remove-duplicates list-insts)
          (remove-duplicates list-entries)
          (remove-duplicates list-saved)
          list-sources)))

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    (let ((assembled (assemble controller-text machine)))
      ((machine 'install-instruction-sequence) (car assembled))
      ((machine 'install-extras) (cdr assembled)))
    machine))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())

        (sorted-insts '())
        (entry-points '())
        (saved-or-restored '())
        (sources '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if (not val)
              (begin
                (allocate-register name)
                (cadr (assoc name register-table)))
              (cadr val))))
      
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      
      (define (install-extras lst)
        (set! sorted-insts (first lst))
        (set! entry-points (second lst))
        (set! saved-or-restored (third lst))
        (set! sources (list-ref lst 3))
        
        ;; (for-each (lambda (register-name)
        ;;             (allocate-register register-name))
        ;;           (map car sources))
        )
      
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              
              ((eq? message 'install-extras) install-extras)
              ((eq? message 'get-sorted-insts) sorted-insts)
              ((eq? message 'get-entry-points) entry-points)
              ((eq? message 'get-saved-or-restored) saved-or-restored)
              ((eq? message 'get-sources) sources)
              
              (else (error "Unknown request -- MACHINE" message))))
      
      dispatch)))

(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (let ((extras (update-insts! insts labels machine)))
                      (cons insts extras)))))

;; recursive factorial machine from fig. 5.11
(define recur-fact-machine
  (make-machine
   '()
   (list (list '* *) (list '- -) (list '= =))
   '((assign continue (label fact-done))     ; set up final return address
    fact-loop
    (test (op =) (reg n) (const 1))
    (branch (label base-case))
    ;; Set up for the recursive call by saving n and continue.
    ;; Set up continue so that the computation will continue
    ;; at after-fact when the subroutine returns.
    (save continue)
    (save n)
    (assign n (op -) (reg n) (const 1))
    (assign continue (label after-fact))
    (goto (label fact-loop))
    after-fact
    (restore n)
    (restore continue)
    (assign val (op *) (reg n) (reg val))   ; val now contains n(n - 1)!
    (goto (reg continue))                   ; return to caller
    base-case
    (assign val (const 1))                  ; base case: 1! = 1
    (goto (reg continue))                   ; return to caller
    fact-done)))
