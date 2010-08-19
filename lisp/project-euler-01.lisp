; Project Euler

; Add all the natural numbers below one thousand that are multiples of 3 or 5.
(do ((n 0 (1+ n))
	      (sum 0 (if (or (= 0 (mod n 3))
			      (= 0 (mod n 5)))
			 (+ sum n)
			 sum)))
	     ((= n 1000) sum))