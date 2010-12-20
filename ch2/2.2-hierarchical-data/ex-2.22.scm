;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 2 Section 2 Hierarchical Data and the Closure Property

;;; Exercise 2.22

;; "Louis Reasoner's" function for square-list is the same
;; as my iterative reverse function, except it square each item as well.
;; This could be enough to explain why it reverses the list...
;;
;; At the first level of recursion of iter, the parameter answer is
;; nil. When iter is called recursive, (cons (square (car things)) answer)
;; is the parameter answer. This means the first item in the list things ends
;; up as the "deepest" cons cell, and each item following ends up on top of it,
;; resulting in a reversed list.
;;
;; The second procedure with the reversed calls to cons creates an even
;; worse solution, because during the first call to iter, it conses
;; answer which is a list onto an integer, resulting in a sort of backwards
;; or inverted structure...
;
;;> (square-list (list 1 2 3 4))
;; ((((() . 1) . 4) . 9) . 16)
;;
;; However, I'm not sure how to create an iterative map.  I started to think
;; about using a "pointer" to the last element of the list, but I am pretty 
;; sure you would need to use set! or something to modify the cdr of whatever
;; that was pointing to...

