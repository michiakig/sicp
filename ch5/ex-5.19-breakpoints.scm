;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 5 Section 2 A Register Machine Simulator

;;;; Exercise 5.19 adding breakpoints

;; redefine make-new-machine to support new execute and breakpoint interface
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        ;; new "fields" for the machines
        (labels '())
        (breakpoints '()))
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
      
      ;; new execute for handling breakpoints
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ;; are there any breakpoints set on this instruction?
                (let ((possible (filter (lambda (breakpoint)
                                          (= (breakpoint-absolute breakpoint)
                                             (instruction-line-count (car insts))))
                                        breakpoints)))
                  ;; if not, continue
                  (if (null? possible)
                      (execute-do)
                      ;; otherwise, print out breakpoint info and
                      ;; stop execution -- contents of pc remain 
                      (begin
                        (display "Breakpoint hit -- ")
                        (for-each
                         (lambda (breakpoint)
                           (display (breakpoint-label breakpoint))
                           (display " ")
                           (display (breakpoint-offset breakpoint))
                           (display " "))
                         possible)
                        (newline))))))))
      
      ;; continue procedure, execute the contents of pc and loop
      (define (execute-do)
        (let ((insts (get-contents pc)))
          ((instruction-execution-proc (car insts)))
          (execute)))
      
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
              
              ;; interface for setting and removing breakpoints
              ((eq? message 'set-breakpoint!)
               (lambda (label offset)
                 (set! breakpoints (cons (make-breakpoint label offset labels)
                                         breakpoints))))
              ((eq? message 'cancel-breakpoint!)
               (lambda (label offset)
                 (set! breakpoints
                       (filter (lambda (breakpoint)
                                 (not (and (eq? (breakpoint-label breakpoint)
                                                label)
                                           (eq? (breakpoint-offset breakpoint)
                                                offset))))
                               breakpoints))))
              ((eq? message 'cancel-all-breakpoints!)
               (lambda () (set! breakpoints '())))
              ((eq? message 'proceed)
               (lambda () (execute-do)))
              ((eq? message 'set-labels)
               (lambda (l) (set! labels l)))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

;; external interface for breakpoints 
(define (proceed-machine machine) ((machine 'proceed)))
(define (set-breakpoint! machine label offset)
  ((machine 'set-breakpoint!) label offset))
(define (cancel-breakpoint! machine label offset)
  ((machine 'cancel-breakpoint!) label offset))
(define (cancel-all-breakpoints! machine)
  ((machine 'cancel-all-breakpoints!)))

;; selectors for breakpoint structure -- just a list (label offset absolute-offset)
(define (breakpoint-label breakpoint) (car breakpoint))
(define (breakpoint-offset breakpoint) (cadr breakpoint))
(define (breakpoint-absolute breakpoint) (caddr breakpoint))

;; labels argument is the list of all labels from extract-labels
(define (make-breakpoint label offset labels)
  (let ((record (assoc label labels)))
    (if record
        ;; compute the absolute line number for this breakpoint
        (list label offset (+ offset (label-entry-count record)))
        (error "Unknown label -- MAKE-BREAKPOINT" label))))

;; redefine extract-labels to count instructions and tag instructions
;; and labels with line counts
(define (extract-labels text recieve)
  (define (extract-labels-count count text receive)
    (if (null? text)
        (receive '() '())
        (extract-labels-count
         ;; don't count labels, only instructions
         (if (symbol? (car text)) count (+ 1 count))
         (cdr text)
         (lambda (insts labels)
           (let ((next-inst (car text)))
             (if (symbol? next-inst)
                 (receive insts
                          (cons (make-label-entry next-inst
                                                  insts
                                                  count)
                                labels))
                 (receive (cons (make-instruction next-inst count)
                                insts)
                          labels)))))))
  (extract-labels-count 0 text recieve))

;; redefine update-insts! to install the list of labels into the machine
(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    ;; install list of labels into machine
    ((machine 'set-labels) labels)
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc! 
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))

;; redefinition of instruction to be a list (text procedure count)
(define (make-instruction text count)
  (list text '() count))
;; selectors for instruction 
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst) (cadr inst))
(define (instruction-line-count inst) (caddr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-car! (cdr inst) proc))

;; redefinition of label to be a list (name insts count), selectors
(define (label-entry-name entry) (car entry))
(define (label-entry-inst entry) (cadr entry))
(define (label-entry-count entry) (caddr entry))

(define (make-label-entry label-name insts count)
  (list label-name insts count))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (label-entry-inst val)
        (error "Undefined label -- ASSEMBLE" label-name))))
