#lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
(define segments-painter segments->painter)

;Exercise 2.49.  Use segments->painter to define the following primitive painters:

;a.  The painter that draws the outline of the designated frame.

(define outline 
  (lambda (f)
  (let ((lower-left (make-vect 0 0))
        (upper-left (make-vect 0 1))
        (lower-right (make-vect 1 0))
        (upper-right (make-vect 1 1)))
    ((segments-painter (list (make-segment lower-left upper-left)
                             (make-segment lower-left lower-right)
                             (make-segment upper-left upper-right)
                             (make-segment lower-right upper-right))) f))))

;b.  The painter that draws an ``X'' by connecting opposite corners of the frame.

(define (xcorners f)
  (let ((lower-left (make-vect 0 0))
        (upper-left (make-vect 0 1))
        (lower-right (make-vect 1 0))
        (upper-right (make-vect 1 1)))
    ((segments-painter (list (make-segment lower-left upper-right)
                            (make-segment lower-right upper-left))) f)))
        

;c.  The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.

(define (diamond f)
  (let ((mid-left (make-vect 0 0.5))
        (mid-right (make-vect 1 0.5))
        (mid-bottom (make-vect 0.5 0))
        (mid-top (make-vect 0.5 1)))
    ((segments-painter (list (make-segment mid-left mid-top)
                             (make-segment mid-left mid-bottom)
                             (make-segment mid-right mid-top)
                             (make-segment mid-right mid-bottom))) f)))

;d.  The wave painter.
