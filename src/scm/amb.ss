(define (require pred)
  (or pred (amb)))

(define (amb-range lo step hi)
  (if (>= lo hi) hi (amb lo (amb-range (+ lo step) step hi))))

(try-catch
  (let ((x (amb 0 1 3 5)))
    (require (even? x))
    (require (not (= 0 x)))
    x)
  'all-odd-or-div-by-zero)
