(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5)) x) (cv 32)))

(define c (make-connector))
(probe "Celsius" c)

(define f (celsius-fahrenheit-converter c))
(probe "Fahrenheit" f)

; (set-value! c 100 'user)
; (forget-value! c 'user)
; (set-value! f 32 'user)


(define (ohms-law v r) (c/ v r))

(define v (make-connector))
(probe "Voltage" v)

(define r (make-connector))
(probe "Resistance" r)

(define i (ohms-law v r))
(probe "Current" i)
