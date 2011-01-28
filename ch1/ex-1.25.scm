;;;; Structure and Interpretation of Computer Programs
;;;; Chapter 1 Section 2 Procedures and the Processes They Generate

;;; Exercise 1.25

;; The implementation of expmod using fast-expt will not keep the
;; exponents relatively small, and so it computes (fast-expt base exp)
;; before taking the remainder and this may be a very large
;; number. This would be a very big problem in a language without
;; arbitrary precision integers, where an overflow would occur but in
;; Scheme it would just take longer, due to the overhead of bignum integers
