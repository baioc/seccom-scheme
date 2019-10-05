(define (make-table)
  (cons '*table* '()))

(define (lookup table key)
  (let ((record (assoc key (cdr table))))
    (cond (record => cdr)
          (else #f))))

(define (insert! table key value)
  (cond ((assoc key (cdr table)) => (lambda (record) (set-cdr! record value)))
        (else (set-cdr! table
                        (cons (cons key value) (cdr table)))))
  'ok)

(define (clear-table! table)
  (set-cdr! table '())
  'ok)


; (define (assoc k a-list)
;   (cond ((null? a-list) #f)
;         ((equal? k (caar a-list)) (car a-list))
;         (else (assoc k (cdr a-list)))))

; (define make-table make-hash-table)
; (define lookup hash-ref)
; (define insert! hash-set!)


(define (make-wrapper)
  (let ((x 'void))
    (define (setter! y)
      (set! x y)
      'ok)
    (define (self method)
      (cond ((eq? method 'get) x)
            ((eq? method 'set) setter!)
            (else (error "Undefined operation" method))))
    self))

(define (get-value w)
  (w 'get))

(define (set-value! w x)
  ((w 'set) x))
