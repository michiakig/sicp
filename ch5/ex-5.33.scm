;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 5 Section 5 Compilation

;;; Exercise 5.33

;; The only difference between the source code in question is the
;; ordering of the arguments to * in the recursive case. 

(* n (factorial-alt (- n 1)))

;; In the above code, since the recursive call to factorial-alt is the
;; last argument in the list, and therefore the first argument handled
;; by the construct-arglist procedure. The resulting object code will
;; preserve the env around the call to factorial-alt.

(* (factorial (- n 1)) n)

;; In this code, the call to factorial is the first argument, or the
;; last argument handled in code-to-get-rest-args. In this case, there
;; is no need to preserve the env around this call, but there is a
;; need to preserve argl, since it contains the value of n.

;; I'm not sure which is more efficient, they have the same number of
;; stack saves and restores, so there probably is no difference.

