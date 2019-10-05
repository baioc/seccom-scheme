(define (pow power operation basis neutral succession)
  (let iter ((b basis) (n power) (acc neutral))
    (cond ((= n 0) acc)
          ((even? n) (iter (succession b) (halve n) acc))
          (else (iter b (- n 1) (operation b acc))))))

(define (empower operation neutral . opts-succ)
  (let ((succession (maybe-car opts-succ (lambda (x) (operation x x)))))
    (lambda (basis power)
      (pow power operation basis neutral succession))))


(define (fixpoint f x . opts-tol)
  (let* ((tolerance (maybe-car opts-tol 1e-9))
         (approx? (lambda (a b) (< (abs (- a b)) tolerance))))
    (let try ((old x) (new (f x)))
      (if (approx? old new) new
          (try new (f new))))))

(define (root f x . opts-tol)
  (let* ((dx (maybe-car opts-tol 1e-8))
         (df (deriv f dx)))
    (fixpoint (lambda (x) (- x (/ (f x) (df x)))) x dx)))

(define (deriv f dx)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))


(define (fix f)
  (define (g x)
    (lambda (arg)
      ((f (x x)) arg)))
  (g g))


(define (memoize proc)
  (let ((cache (make-table)))
    (define (delegate . args)
      (let ((hit (lookup cache args)))
        (or hit
            (let ((result (apply proc args)))
              (insert! cache args result)
              result))))
    delegate))
