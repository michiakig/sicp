;;;; Exercise 3.21, 3.22, 3.23

;;; A queue is a cons of the form (<list> . <last item in list>) so this means
;;; that Scheme prints queues out as ((a b) b), where the list representation 
;;; of the queue is (a b) and the last item in the queue is the second b

(define (print-queue queue)
  (display (front-ptr queue))
  'ok)


;;; Implements a queue in terms of a procedure with local state.
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

;;; external facing procedures
(define (insert q item) ((q 'insert) item))
(define (delete q) ((q 'delete)))
(define (front q) ((q 'front)))

;;; A deque is implemented simply as a pair pointing to a doubly-linked list.
;;; The pair representing the deque consist of the car pointing to the first
;;; element and the cdr pointing to the last element.

;;; constructor
(define (make-deque)
  (cons '() '()))

;;; empty predicate
(define (empty-deque? d)
  (null? (front-deque d)))

;;; returns the node at the front
(define (front-deque d)
  (car d))

;;; returns the node at the rear
(define (rear-deque d)
  (cdr d))

;;; predicate to test for a deque with only one element
(define (single-deque? d)
  (null? (next-node (front-deque d))))

(define (front-insert-deque! d item)
  (let ((new-node (make-node! item (front-deque d) '())))
    (cond ((empty-deque? d)
	   (set-car! d new-node)
	   (set-cdr! d new-node))
	  (else
	   (set-car! d new-node)))
    d))

(define (rear-insert-deque! d item)
  (let ((new-node (make-node! item '() (rear-deque d))))
    (cond ((empty-deque? d)
	   (set-car! d new-node)
	   (set-cdr! d new-node))
	  (else
	   (set-cdr! d new-node)))
    d))

(define (front-delete-deque! d)
  (cond ((empty-deque? d)
	 (error "DELETE! on empty deque"))
	((single-deque? d)
	 (set-car! d '())
	 (set-cdr! d '()))
	(else
	 (set-car! d (next-node (front-deque d)))))
  d)

(define (rear-delete-deque! d)
  (cond ((empty-deque? d)
	 (error "DELETE! on empty deque"))
	((single-deque? d)
	 (set-car! d '())
	 (set-cdr! d '()))
	(else
	 (set-cdr! d (prev-node (rear-deque d)))
	 (set-next! (rear-deque d) '())))
  d)

;;; doubly linked list implementation
(define (next-node n) (cddr n))
(define (prev-node n) (cadr n))
(define (item-node n) (car n))

(define (make-node! item next prev)
  (let ((new-node (cons item (cons prev next))))
    (if (not (null? next))
	(set-prev! next new-node))
    (if (not (null? prev))
	(set-next! prev new-node))
    new-node))

(define (set-next! n x) (set-cdr! (cdr n) x))
(define (set-prev! n x) (set-car! (cdr n) x))

;;; convert a doubly-linked list to a regular list
(define (double-to-list lst)
  (cond ((null? lst) '())
	(else (cons (item-node lst)
		    (double-to-list (next-node lst))))))

