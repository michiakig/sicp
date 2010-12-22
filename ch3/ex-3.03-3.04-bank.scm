;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 3 Section 1 Assignment and Local State

;;;; Exercise 3.3

;;; creates password protected accounts and keeps track of the number of failed
;;; attempts to access the account.

(define (make-account balance passwd)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

;;;; Exercise 3.4

  (define dispatch 
    (let ((attempts 0))
      (lambda (p m)
        (cond ((eq? passwd p)
               (set! attempts 0)
               (cond ((eq? m 'withdraw) withdraw)
                     ((eq? m 'deposit) deposit)
                     (else (error "Unknown request -- MAKE-ACCOUNT" m))))
              (else 
                (cond ((>= attempts 7) (call-the-cops))
                      (else
                        (set! attempts (+ attempts 1))
                        (error "Incorrect password."))))))))

  (define (call-the-cops)
    (error "alarum alarum!"))

  dispatch)
