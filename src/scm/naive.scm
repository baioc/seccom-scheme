(define (c2f x)
  (+ (* (/ 9 5) x) 32))

(define (gcd a b)
  (if (= b 0) a
      (gcd b (modulo a b))))

(define (fac n)
  (if (= n 0) 1
      (* n (fac (- n 1)))))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (2^ n)
  (cond ((= n 0) 1)
        ((= n 1) 2)
        (else (+ (2^ (- n 1))
                 (2^ (- n 1))))))
