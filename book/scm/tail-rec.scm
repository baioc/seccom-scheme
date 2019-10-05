(define (factorial n)
  (let iter ((n n) (prod 1))
    (if (<= n 1) prod
        (iter (- n 1) (* n prod)))))

(define (fibo n)
  (let iter ((n n) (prev 1) (curr 0))
    (if (= n 0) curr
        (iter (- n 1) curr (+ prev curr)))))


; (define (^ b n)
;   (if (= n 0) 1 (* b (^ b (- n 1)))))

; (define (^ b n)
;   (let iter ((n n) (prod 1))
;     (if (= n 0) prod
;         (iter (- n 1) (* b prod)))))

; (define (^ b n)
;   (cond ((= n 0) 1)
;         ((even? n) (square (^ b (halve n))))
;         (else (* b (^ b (- n 1))))))

(define (^ b n)
  (define (iter b n prod)
    (cond ((= n 0) prod)
          ((even? n) (iter (square b) (halve n) prod))
          (else (iter b (- n 1) (* b prod)))))
  (if (< n 0)
      (iter (/ 1 b) (- n) 1)
      (iter b n 1)))


(define (mul b n)
  (define (iter b n acc)
    (cond ((= n 0) acc)
          ((even? n) (iter (double b) (halve n) acc))
          (else (iter b (- n 1) (+ b acc)))))
  (if (< n 0)
      (iter (- b) (- n) 0)
      (iter b n 0)))


(define (sqrt x)
  (define (try guess)
    (if (good-enough? guess) guess
        (try (improve guess))))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) tolerance))
  (try 1.0))

(define (fixcos)
  (define (retry old new)
    (if (approx? new old) new
        (retry new (cos new))))
  (define (approx? a b)
    (< (abs (- a b)) tolerance))
  (retry 0.0 (cos 0.0)))
