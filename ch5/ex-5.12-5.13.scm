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

(define (assign-reg-target inst) (cadr (instruction-text inst)))
(define (assign-reg-src inst) (caddr (instruction-text inst)))

;; used in ex 13 to allocated registers during assembly
(define (get-registers-referenced inst)
  (let ((rest (map cadr (filter (lambda (arg)
                                (eq? (car arg) 'reg))
                                (cddr (instruction-text inst))))))
    (if (or (eq? (instruction-text-op inst) 'assign)
            (eq? (instruction-text-op inst) 'save)
            (eq? (instruction-text-op inst) 'restore))
        (cons (assign-reg-target inst)
              rest)
        rest)))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations))
        
        (list-insts '())
        (list-entries '())
        (list-saved '())
        (list-sources '())

        (registers-seen '()))
    
    (for-each
     (lambda (inst)

       ;; for ex 12 
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
       
       (if (eq? (instruction-text-op inst) 'assign)
           (let ((record (assoc (assign-reg-target inst) list-sources)))
             (if record
                 (set-cdr! record (cons (assign-reg-src inst) (cdr record)))
                 (set! list-sources
                       (cons (cons (assign-reg-target inst)
                                   (list (assign-reg-src inst)))
                             list-sources)))))

       ;; ex 12, if the inst contains a register we haven't seen yet,
       ;; allocate it
       (for-each
        (lambda (reg)
          ((machine 'allocate-register) reg)
          (set! registers-seen (cons reg registers-seen)))
        (remove-duplicates (filter (lambda (reg)
                                     (not (member reg registers-seen)))
                                   (get-registers-referenced inst))))
       
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
    ;; for ex 13, we allocate registers during assemble instead
    ;; (for-each (lambda (register-name)
    ;;             ((machine 'allocate-register) register-name))
    ;;           register-names)
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
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      
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
        (set! sources (list-ref lst 3)))
      
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

