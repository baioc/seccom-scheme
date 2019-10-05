(define (square x)
  (* x x))

(define (halve x)
  (ash x -1))

(define (double x)
  (ash x 1))

(define (average a b)
  (/ (+ a b) 2))

(define (maybe-car lst alt)
  (if (null? lst) alt
      (car lst)))

(define tolerance 1e-15)
