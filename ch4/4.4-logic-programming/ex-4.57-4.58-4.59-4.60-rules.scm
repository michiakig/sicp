;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 4 Section 4 Logic Programming

;;;; Exercise 4.57

(rule (can-replace ?person-1 ?person-2)
      (and (or (and (job ?person-1 ?job)
                    (job ?person-2 ?job))
               (and (job ?person-1 ?job-1)
                    (job ?person-2 ?job-2)
                    (can-do ?job-1 ?job-2)))
           (not (same ?person-1 ?person-2))))

;; a. all people who can replace Cy D. Fect;

(can-replace ?person (Fect Cy D))

;; b. all people who can replace someone who is being paid more then
;; they are

(and (salary ?person-1 ?salary-1)
     (salary ?person-2 ?salary-2)
     (lisp-value > ?salary-2 ?salary-1)
     (can-replace ?person-1 ?person-2))

;;;; Exercise 4.58

(rule (big-shot ?person)
      (and (job ?person (?person-division . ?rest-1))
           (supervisor ?person ?supervisor)
           (job ?supervisor (?supervisor-division . ?rest-2))
           (not (same ?person-division ?supervisor-division))))

;;;; Exervise 4.59

;; a.

(meeting ?name (Friday ?time))

;; b.

(rule (meeting-time ?person ?day-and-time)
      (and (job ?person (?division . ?rest))
           (meeting ?division ?day-and-time)))

;; c.

(meeting-time (Hacker Alyssa P) (Wednesday ?time))

;;;; Exercise 4.60

(rule (lives-near-2 ?person)
      (lives-near ?person ?x))
