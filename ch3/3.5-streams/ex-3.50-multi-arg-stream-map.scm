;;;; Ex. 3.50 Complete the definition of stream-map to support multiple arguments

;;; analogous to map in s2.2.3, footnote 12

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams)) ; check if the first stream is null
      the-empty-stream
      (stream-cons ; I think we want stream-cons here
       (apply proc (map car argstreams))
       (apply stream-map
	      (cons proc (map cdr argstreams))))))


