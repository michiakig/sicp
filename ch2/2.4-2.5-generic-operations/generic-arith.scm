;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 2 Section 4 Multiple Representations for Abstract Data
;;;; Chapter 2 Section 5 Systems with Generic Operations

;;;; 2.5.1 Generic Arithmetic Operations

;;; The majority of this code is straight from the text, except where noted for 
;;; certain snippets where exercises are implemented.

;;; The definitions below form the "public API" of the system.
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;; ex 2.79, 2.80
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

;; ex 2.83
(define (raise x) (apply-generic 'raise x))
;; ex 2.84
(define (height x) (apply-generic 'height x))
;; ex 2.88
(define (negate x) (apply-generic 'negate x))

;; These are the constructors for individual types.
(define (make-scheme-number n)
  ((get 'make '(scheme-number)) n))

(define (make-rational n d)
  ((get 'make '(rational)) n d))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

;;; The rest of the file includes the three definitions for installing the
;;; different packages: Scheme (basic), rational, and complex numbers.

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))    

  ;; ex 2.79, 2.80
  (put '=zero? '(scheme-number)
       (lambda (x) (= 0 x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))

  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make '(scheme-number)
       (lambda (x) (tag x)))
  ;; ex 2.83
  (put 'raise '(scheme-number)
       (lambda (x) (make-rational x 1)))
  ;; ex 2.84
  (put 'height '(scheme-number)
       (lambda (x) 0))
  ;; ex 2.88
  (put 'negate '(scheme-number)
       (lambda (x) (* -1 x)))
  'done)

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))

  ;; ex 2.79, 2.80
  (put '=zero? '(rational)
       (lambda (x)
          (= 0 (numer x))))
  (put 'equ? '(rational rational)
       (lambda (x y)
          (and (= (numer x) (numer y))
               (= (denom x) (denom y)))))

  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make '(rational)
       (lambda (n d) (tag (make-rat n d))))

  ;; ex 2.83
  (put 'raise '(rational)
       (lambda (r) (make-complex-from-real-imag (/ (numer r) (denom r)) 0)))
  ;; ex 2.84
  (put 'height '(rational)
       (lambda (r) 1))
  ;; ex 2.88
  (put 'negate '(rational)
       (lambda (r) (make-rational (negate (numer r))
                                  (denom r))))
  'done)

;; Installing this package depends on the rectangular and polar packages having been 
;; installed already
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  ;; I think you need to have installed the rect and polar packages first, right?
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))

  ;; ex 2.79, 2.80
  (put 'equ? '(complex complex)
       (lambda (z1 z2)
         (and (= (magnitude z1) (magnitude z1))
              (= (angle z1) (angle z2)))))
  (put '=zero? '(complex)
       (lambda (z)
         (and (= 0 (magnitude z))
              (= 0 (angle z)))))

  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  ;; ex 2.83
  (put 'raise '(complex)
       (lambda (c) (tag c)))
  ;; ex 2.84
  (put 'height '(complex)
       (lambda (c) 2))
  ;; ex 2.88
  (put 'negate '(complex)
       (lambda (c) (sub-complex (make-from-real-imag 0 0) c)))
  
  ;; ex 2.77
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

