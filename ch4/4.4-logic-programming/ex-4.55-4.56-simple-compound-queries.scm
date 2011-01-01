;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 4 Section 4 Logic Programming

;;;; Exercise 4.55

;; a. all people supervised by Ben Bitdiddle;

(supervisor ?x (Bitdiddle Ben))

;; b. the names and jobs of all people in the accounting division;

(job ?x (accounting . ?y))

;; c. the names and addresses of all the people who live in
;; Slumerville

(address ?x (Slumerville . ?y))

;;;; Exercise 4.56

;; a. the names of all people who are supervised by Ben Bitdiddle,
;; together with their addresses;

(and (supervisor ?person (Ben Bitdiddle))
     (address ?person ?where))

;; b. all people whose salary is less than Ben Bitdiddles's, together
;; with their salary and Ben Bitdiddle's salary;

(and (salary ?person ?other)
     (salary (Ben Bitdiddle) ?ben)
     (lisp-value > ?ben ?other))

;; c. all people who are supervised by someone who is not in the
;; computer division, together with the supervisor's name and job

(and (supervisor ?person ?supervisor)
     (not (job ?supervisor (computer . ?type))))

