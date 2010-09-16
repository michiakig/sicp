;; Exercise 3.21

; A queue is a cons of the form (<list> . <last item in list>) so this means that
; Scheme prints queues out as ((a b) b), where the list representation of the queue
; is (a b) and the last item in the queue is the second b

(define (print-queue queue)
  (display (front-ptr queue))
  'ok)

; from the text...
(define (make-queue-proc)
  (let ((front-ptr '())
	(rear-ptr '()))

    (define (insert item)
      (let ((new-pair (cons item '())))
	(cond ((null? front-ptr)
	       (set! front-ptr (cons new-pair front-ptr))
	       (set! rear-ptr front-ptr))
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set! rear-ptr new-pair)
	       queue))))

    (define (delete)
      (cond ((null? front-ptr)
	     (error "DELETE! called on an empty queue"))
	    (else
	     (set! front-ptr (cdr (front-ptr queue)))
	     queue)))

    (define (dispatch m)
      (cond ((eq? m 'front) (car front-ptr))
	    ((eq? m 'insert) insert)
	    ((eq? m 'delete) delete)))
    dispatch))

;; Exercise 3.22