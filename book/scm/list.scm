(define (stream-ref seq n)
  (cond ((empty? seq) #f)
        ((<= n 0) (head seq))
        (else (stream-ref (tail seq) (- n 1)))))

(define (stream-for-each proc seq)
  (if (not (empty? seq))
      (begin
        (proc (head seq))
        (stream-for-each proc (tail seq)))))

(define (stream-map proc seq)
  (if (empty? seq) empty-stream
      (stream (proc (head seq))
              (stream-map proc (tail seq)))))

(define (stream-filter pred seq)
  (cond ((empty? seq) empty-stream)
        ((pred (head seq)) (stream (head seq)
                                   (stream-filter pred (tail seq))))
        (else (stream-filter pred (tail seq)))))

(define (stream-foldr op acc seq) ;; aka reduce
  (if (empty? seq) acc
      (op (head seq)
          (stream-foldr op acc (tail seq)))))

(define (stream-zip-with op sa sb)
  (stream (op (head sa) (head sb))
          (stream-zip-with op (tail sa) (tail sb))))
