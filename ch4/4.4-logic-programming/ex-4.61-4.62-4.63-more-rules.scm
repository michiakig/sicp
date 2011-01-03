;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 4 Section 4 Logic Programming

;;;; Exercise 4.61

> (?x next-to ?x in (1 (2 3) 4))

(1 next-to (2 3) (1 (2 3) 4))
((2 3) next-to 4 (1 (2 3) 4))

> (?x next-to 1 in (2 1 3 1))

(2 next-to 1 in (2 1 3 1))

It will also find: (3 next-to 1 in (2 1 3 1))

;;;; Exercise 4.62

(rule (last-pair (?x . ()) (?x)))

(rule (last-pair (?y . ?z) (?x))
      (last-pair ?z (?x)))

;; These rules work for queries like:

> (last-pair (3) ?x)
> (last-pair (1 2 3) ?x)
> (last-pair (2 ?x) (3))

;; ... but do not work for:

> (last-pair ?x (3))

;; There are an infinite number of lists with the last pair (3)

;;;; Exercise 4.63

(rule (grandson ?s ?g)
       (and (son ?f ?s)
            (son ?g ?f)))

(rule (son-of ?s ?m)
      (or (son ?m ?s)
          (and (wife ?m ?w)
               (son ?w ?s))))
