; Louis Reasoner is having great difficulty doing exercise 1.24. His fast-prime? test seems to run more slowly than his prime? test. Louis calls his friend Eva Lu Ator over to help. When they examine Louis's code, they find that he has rewritten the expmod procedure to use an explicit multiplication, rather than calling square:

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)    ; The problem is that here the function is called recursively twice
                       (expmod base (/ exp 2) m))   ; instead of only a single time and returning a result which is squared
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))


