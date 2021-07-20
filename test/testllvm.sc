

(define perm (lambda (n)
	(if (= n 0) 1 (* n (perm (- n 1)) ))))
	    
(showInt (perm 1))
(showInt (perm 2))
(showInt (perm 3))
(showInt (perm 4))
(showInt (perm 5))
(showInt (perm 10))


(define fib
    (lambda (n)
      (if (< n 2)
	  1
	  (+ (fib (- n 1))
	     (fib (- n 2))))))

(showInt (fib 10))




