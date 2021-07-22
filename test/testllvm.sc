

(define a 1)
(define b 2)
(define c "Hello, LLVM3")

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

(define plus (lambda (n m) (+ n m)))

(showInt (plus 10 20))

(showInt (plus a b))

(showStr "Hello, LLVM1")

(showStr "Hello, LLVM2")

(showStr c)






