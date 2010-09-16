;; Exercise 3.21

; A queue is a cons of the form (<list> . <last item in list>) so this means that
; Scheme prints queues out as ((a b) b), where the list representation of the queue
; is (a b) and the last item in the queue is the second b

(define (print-queue queue)
  (display (front-ptr queue))
  'ok)

;; Exercise 3.22
(define (make-queue-proc)
  (let ((front-ptr '())
	(rear-ptr '()))

    (define (insert item)
	(cond ((null? front-ptr)
	       (set! front-ptr (cons item front-ptr))
	       (set! rear-ptr front-ptr)
	       front-ptr)
	      (else
	       (set-cdr! rear-ptr (cons item '()))
	       (set! rear-ptr (cdr rear-ptr))
	       front-ptr)))

    (define (delete)
      (cond ((null? front-ptr)
	     (error "DELETE! called on an empty queue"))
	    (else
	     (set! front-ptr (cdr front-ptr))
	     front-ptr)))

    (define (front)
      (cond ((null? front-ptr)
	     (error "FRONT called on an empty queue"))
	    (else
	     (car front-ptr))))

    (define (dispatch m)
      (cond ((eq? m 'front) front)
	    ((eq? m 'insert) insert)
	    ((eq? m 'delete) delete)))
    dispatch))