(define (memo-proc proc)
  (let ((run? #f)
        (cached '()))
    (lambda ()
      (if run? cached
          (let ((result (proc)))
            (begin
              (set! run? #t)
              (set! cached result)
              result))))))

(define-syntax lazy
  (syntax-rules ()
    ((lazy expr)
      (memo-proc (lambda () expr)))))

(define (thunk p)
  (p))

(define-syntax stream
  (syntax-rules ()
    ((stream x y)
      (cons x (lazy y)))))

(define (head s)
  (car s))

(define (tail s)
  (thunk (cdr s)))

(define (empty? s)
  (null? s))

(define empty-stream '())
