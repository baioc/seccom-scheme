(define (fibonac n)
  (define phi (/ (+ 1 (sqrt 5)) 2))
  (define fi (- 1 phi))
  (round (/ (- (^ phi n) (^ fi n))
            (sqrt 5))))


(define x (fixpoint cos 0))

(define (phi-rat tol)
  (fixpoint
    (lambda (rat)
      (let ((fcurr (numerator rat))
            (fprev (denominator rat)))
        (/ (+ fcurr fprev) fcurr)))
    1/1
    tol))

(define phi (exact->inexact (phi-rat 1e-15)))

; (define (average-damp f)
;   (lambda (x) (average (f x) x)))
;
; (define (sqrt x)
;   (fixpoint (average-damp (lambda (y) (/ x y))) 1.0))


(define (sqrt x)
  (root (lambda (y) (- x (square y))) 1.0))

(define pi (root sin 3))
