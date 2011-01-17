;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 5 Section 2 A Register-Machine Simulator

;;;; Exercise 4.8

(define dupe-labels-machine
  (make-machine
   '(a)
   '()
   '(start
     (goto (label here))
     here
     (assign a (const 3))
     (goto (label there))
     here
     (assign a (const 4))
     (goto (label there))
     there)))

;; This is a bit confusing, due to the continuation-passing-style used
;; in extract-labels. Lookup-label uses assoc to lookup the label in
;; the labels list, which will return the first instance matching the
;; key. In this case, it's the first here label, which results in 3
;; being assigned to a, and the jump to the there label.

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (if (assoc next-inst labels)
                   (error "EXTRACT-LABELS -- DUPLICATE LABEL" next-inst)
                   (receive insts
                            (cons (make-label-entry next-inst
                                                    insts)
                                  labels)))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))

