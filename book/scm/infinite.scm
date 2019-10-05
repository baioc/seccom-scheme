(define (ints-from n)
  (stream n (ints-from (+ n 1))))

(define (sieve seq)
  (define (divisible? a b)
    (= 0 (remainder a b)))
  (if (empty? seq) seq
      (stream (head seq)
              (sieve (stream-filter (lambda (x) (not (divisible? x (head seq))))
                                    (tail seq))))))

(define primes (sieve (ints-from 2)))


(define (arctan-series x n)
  (stream (/ (expt x n) n)
          (stream-map - (arctan-series x (+ n 2)))))

(define (partial-sums s)
  (stream-zip-with + s (stream 0 (partial-sums s))))

(define pi-approximations
  (stream-map (lambda (pi/4) (* 4 pi/4))
              (partial-sums (arctan-series 1.0 1))))


(define (fibonacci-sequence prev curr)
  (stream prev
          (fibonacci-sequence curr (+ prev curr))))

(define fibs (fibonacci-sequence 0 1))
