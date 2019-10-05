## Objetos Dinâmicos

No exemplo anterior utilizamos uma lista cujo estado interno era acessado e alterado através de funções auxiliares: os processos são totalmente **orientados aos dados** manipulados.

A abordagem **orientada a objetos** em Scheme envolve a utilização de *closures* que têm acesso ao seu próprio estado e fornecem interfaces para interagir com o ambiente externo.

```scheme
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
```

Nesse exemplo, ao passar a mensagem `'set` a um objeto criado por `make-wrapper` o mesmo retorna um procedimento que alterará seu estado interno quando chamado.

Assim, podemos formar sistemas como conjuntos de objetos intercomunicantes.
Note que agora o fluxo de dados não têm sentido único, podendo se propagar de qualquer forma em uma rede de objetos conectados arbitrariamente.

Uma aplicação desse tipo de sistema é encontrada em equações algébricas.
Por exemplo, a equação \\( F = \frac{9}{5} * C + 32 \\) determina a relação entre temperaturas das escalas Celsius e Fahrenheit.

```scheme
(define (c2f x)
  (+ (* (/ 9 5) x) 32))
```

Esse procedimento representa apenas um sentido da igualdade, visto que não permite calcular a temperatura em Celsius dado uma em Fahrenheit.
O mesmo se aplicaria se fossêmos representar a relação da lei de Ohm.

\\[ I = \frac{V}{R}  \Leftrightarrow  V = R * I  \Leftrightarrow  R = \frac{V}{I} \\]

A mesma deve valer para quaisquer valores de corrente, resistência e tensão, onde uma alteração de um lado da igualdade será propagada pela equação e causará uma mudança no lado oposto.
Portanto, visamos modelar equações algébricas através de um [sistema de propagação de restrições](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_sec_3.3.5).
Para isso, vamo inserir em Scheme uma linguagem que trata de conectores e operadores.

```scheme
(define (make-connector)
  (let ((value #f)
        (informant #f)
        (constraints '()))
    (define (set-my-value newval source)
      (cond ((not (has-value? self))
             (set! value newval)
             (set! informant source)
             (for-each-except source
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin
            (set! informant #f)
            (for-each-except retractor
                             inform-about-no-value
                             constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? self)
          (inform-about-value new-constraint))
      'done)
    (define (self request)
      (cond ((eq? request 'has-value?) informant)
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown request -- CONNECTOR" request))))
    self))

(define (for-each-except exception procedure list)
  (let loop ((items list))
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else
           (procedure (car items))
           (loop (cdr items))))))

(define (has-value? connector)
  (if (connector 'has-value?) #t #f))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (constant value connector)
  (define (self request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector self)
  (set-value! connector value self)
  self)

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       self))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       self))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       self))))
  (define (process-forget-value)
    (forget-value! sum self)
    (forget-value! a1 self)
    (forget-value! a2 self)
    (process-new-value))
  (define (self request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request -- ADDER" request))))
  (connect a1 self)
  (connect a2 self)
  (connect sum self)
  self)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 self))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       self))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       self))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       self))))
  (define (process-forget-value)
    (forget-value! product self)
    (forget-value! m1 self)
    (forget-value! m2 self)
    (process-new-value))
  (define (self request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 self)
  (connect m2 self)
  (connect product self)
  self)

(define (probe name connector)
  (define (print-probe value)
    (display name)
    (display " = ")
    (display value)
    (newline))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (self request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request -- PROBE" request))))
  (connect connector self)
  self)

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (cv val)
  (let ((c (make-connector)))
    (constant val c)
    c))

(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))
```

Finalmente, para utilizar o sistema:

```scheme
(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5)) x) (cv 32)))

(define c (make-connector))
(probe "Celsius" c)

(define f (celsius-fahrenheit-converter c))
(probe "Fahrenheit" f)

(set-value! c 100 'user)


(define (ohms-law v r) (c/ v r))

(define v (make-connector))
(probe "Voltage" v)

(define r (make-connector))
(probe "Resistance" r)

(define i (ohms-law v r))
(probe "Current" i)
```
