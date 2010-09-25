;; Exercise 3.21

; A queue is a cons of the form (<list> . <last item in list>) so this means that
; Scheme prints queues out as ((a b) b), where the list representation of the queue
; is (a b) and the last item in the queue is the second b

(define (print-queue queue)
  (display (front-ptr queue))
  'ok)

;; Exercise 3.22

; Implements a queue in terms of a procedure with local state.
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

; external facing procedures
(define (insert q item) ((q 'insert) item))
(define (delete q) ((q 'delete)))
(define (front q) ((q 'front)))

;; Exercise 3.23

; Implementation of a double ended queue

; dequeue is a list - either the empty list when empty or a list containing three
; elements: if it has one element, the first and third point to the same element
; and the second is set to '(). if it has more than one, the first points to the 
; front of the dequeue, the second to the second to last element, and the third to
; the rear (last element)

(define (make-dequeue) (list))
(define (empty-dequeue? q) (null? q))

(define (front-deqeue q)
  (if (empty-dequeue? q)
      (error "FRONT called on empty dequeue")
      (caar q)))

(define (rear-dequeue q)
  (if (empty-dequeue q)
      (error "REAR called on empty dequeue")
      (third q)))
  
(define (front-insert-dequeue! q item)
  (let ((new-pair (cons item '())))
    (cond ((empty-dequeue? q)
	   (list new-pair '() new-pair))
	  (else
	     (if (null? (second q))
		 (set-cdr! new-pair (car q))
		 (set-car! q new-pair)	; set the first to new-pair
		 (set-cad)))
	   (set-cdr! new-pair (first q))
	   (set-car! q new-pair)
	   q))))


(define (front-delete-dequeue! q)
  (cond ((empty-dequeue? q)
	 (error "DELETE! called on empty queue"))
	(else
	 (set-car! q (cdar q))
	 q)))

(define (rear-insert-dequeue! q item)
  (let ((new-pair (cons item '())))
    (cond ((empty-dequeue? q)
	   (set-car! q new-pair)
	   (set-cdr! q (car q))
	   q)
	  (else
	   (let ((end (cdr q)))
	     (set-cdr! end new-pair)
	     (set-cdr! q new-pair)
	     q)))))

(define (rear-delete-dequeue! q)
  (cond ((empty-dequeue? q)
	 (error "DELETE! called on an empty queue"))
	(else
	 ())))