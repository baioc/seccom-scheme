## Potenciação Rápida

Por exemplo, ao procurar abstrair a otimização dos procedimentos de exponenciação inteira e da multiplicação egípcia, podemos destacar suas partes comuns.
Ao fazê-lo, é possível imaginar que estamos calculando um tipo de "potência" para uma operação básica (onde a potenciação aditiva equivale à multiplicação e a potenciação multiplicativa equivale à exponenciação) em que acumulamos o resultado de consecutivas aplicações dessa operação entre uma base e o resultado acumulado até então.
A otimização emerge de alguma propriedade da operação que permite torná-la "mais potente" através de alguma transformação em sua base.

```scheme
(define (pow power operation basis neutral succession)
  (let iter ((b basis) (n power) (acc neutral))
    (cond ((= n 0) acc)
          ((even? n) (iter (succession b) (halve n) acc))
          (else (iter b (- n 1) (operation b acc))))))
```

```scheme
;; PS:
(define (halve x) (ash x -1))
(define (double x) (ash x 1))
(define (square x) (* x x))
(define (maybe-car lst alt)
  (if (null? lst) alt
      (car lst)))
```

O procedimento `pow` recebe todos os parâmetros necessários para computar a potência, inclusive outros procedimentos que são chamados durante a sua execução.
Entretanto, seria interessante encapsular algumas dessas informações em uma dada operação potencializada, que por conta própria efetuaria a computação para qualquer base e expoente desejados.
Essa tarefa pode ser cumprida por um [*closure*](https://en.wikipedia.org/wiki/Closure_(computer_programming)) - um tipo de procedimento que "se lembra" do escopo onde foi definido.
Segue abaixo o exemplo de uma função que utiliza `pow` mas não calcula nada e apenas retorna um *closure* contendo a operação potencializada.

```scheme
(define (empower operation neutral . opts-succ)
  (let ((succession (maybe-car opts-succ (lambda (x) (operation x x)))))
    (lambda (basis power)
      (pow power operation basis neutral succession))))
```

```scheme
(define times (empower + 0))

(define (mul b n)
  (if (< n 0)
      (times (- b) (- n))
      (times b n)))

(define (^ b n)
  (let ((raise (empower * 1)))
    (if (< n 0)
        (raise (/ 1 b) (- n))
        (raise b n))))

(define (fibonacci n)
  (let ((fn (cadr (pow ;; n-esima potencia
                       (abs n)
                       ;; da transformacao
                       (lambda (coefs fibs)
                         (let ((p (car coefs)) (q (cadr coefs))
                               (a (car fibs)) (b (cadr fibs)))
                           (list (+ (* b q) (* a (+ q p)))
                                 (+ (* b p) (* a q)))))
                       ;; a partir de uma base
                       '(0 1)
                       ;; acumulada sobre
                       '(1 0)
                       ;; onde a quadratura da base que
                       ;; regula a potencia da operacao eh
                       (lambda (coefs)
                         (let ((p (car coefs)) (q (cadr coefs)))
                           (list (+ (square q) (square p))
                                 (+ (square q) (* 2 (* p q))))))))))
    ;; "negafibonacci"
    (if (and (< n 0)
             (even? n))
        (- fn)
        fn)))

;; fibonacci matricial
(define (fibona n) ;; n > 0
  (define (mul-matrix-2x2 A B)
    (let ((a11 (caar A)) (a12 (cadar A))
          (a21 (caadr A)) (a22 (cadadr A))
          (b11 (caar B)) (b12 (cadar B))
          (b21 (caadr B)) (b22 (cadadr B)))
      (list (list (+ (* a11 b11) (* a12 b21))
                  (+ (* a11 b12) (* a12 b22)))
            (list (+ (* a21 b11) (* a22 b21))
                  (+ (* a21 b12) (* a22 b22))))))
  (let ((nth-transform (empower mul-matrix-2x2
                                '((1 0)
                                  (0 1)))))
    (caar
      (nth-transform '((1 1)
                       (1 0))
                     (- n 1)))))

;; forma fechada
(define phi (/ (+ 1 (sqrt 5)) 2)) ;; numero de ouro
(define fi (- 1 phi)) ;; complemento de phi
(define (fibonac n)
  (round (/ (- (^ phi n) (^ fi n))
            (sqrt 5))))
```

> Compare o tempo de execução de `fibo` com `fibonacci` ou `fibona` para calcular o milionésimo (n = 1000000) número da sequência.
> Depois, faça o mesmo para `fibonac`.
