;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 5 Section 2 A Register Machine Simulator

;;;; Exercise 5.15 instruction counting
;;;; Exercise 5.16 instruction tracing

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        ;; ex 5.15 counting instructions
        (instruction-count 0)
        ;; ex 5.16 instruction tracing, off by default
        (instruction-tracing-on #f))
    
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
                ;; ex 5.15 counting instructions
                (set! instruction-count (+ instruction-count 1))
                ;; ex 5.16 instruction tracing
                (if instruction-tracing-on
                    (begin
                      (display "TRACE: ")
                      (display (instruction-text (car insts)))
                      (newline)))
                (execute)))))
      
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

              ;; ex 5.15 counting instructions
              ((eq? message 'get-count) instruction-count)
              ((eq? message 'reset-count)
               (lambda () (set! instruction-count 0)))

              ;; ex 5.16 instruction tracing
              ((eq? message 'trace-on)
               (set! instruction-tracing-on #t)
               'instruction-tracing-on)
              ((eq? message 'trace-off)
               (set! instruction-tracing-on #f)
               'instruction-tracing-off)
              
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))
