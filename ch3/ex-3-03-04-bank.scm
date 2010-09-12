;; Exercise 3.3
;; Modify the make-account procedure so that it creates password protected accounts.
;; Exercise 3.4
;; Modify the make-account procedure by adding another local state variable so that,
;; if an account has been accessed with more than 7 consecutive times with an incorrect
;; password, it invokes the procedure call-the-cops.

(define (make-account balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

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
