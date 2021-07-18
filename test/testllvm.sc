

(define perm (lambda (n)
	(if (- n 0) 1 (* n (perm (- n 1)) ))))
	    
(define xxx  (lambda () (perm 10)))


