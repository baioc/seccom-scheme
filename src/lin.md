## Recursão Não-Linear e Linearização

Outras classificações de recursão incluem **recursão mútua** (`odd?` e `even?` definidas anteriormente) e **recursão múltipla** - onde a função é invocada em seu próprio corpo mais de uma vez.
Um exemplo famoso de recursão múltipla binária é a função que calcula os números da [sequência de Fibonacci](https://en.wikipedia.org/wiki/Fibonacci_number):

```scheme
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
```

A árvore de chamadas recursivas gerada é bem maior nesse caso:

```scheme
                            (fib 4)
          (+ (fib 3)                     (fib 2))
    (+ (fib 2)     (fib 1) )       (+ (fib 1) (fib 0))
(+ (fib 1) (fib 0))    1                 1       0
      1       0
       (+ 1 0)         1              (+ 1       0)
              (+ 1 1)                        1
                            (+ 2 1)
                               3
```

Para estimar a complexidade do processo, observe que o número de vezes em que o caso base da recursão é atingido na execução de `(fib n)` é exatamente igual ao (n+1)ésimo número de Fibonacci.
Isso significa que esse procedimento é calculado em **tempo exponencial** proporcional à \\( \phi ^ {n+1} \\), onde \\(\phi\\) é o [número de ouro](https://en.wikipedia.org/wiki/Golden_ratio).

Um primeiro passo para otimizar a computação é expressar o procedimento em uma iteração linear:

```scheme
(define (fibo n)
  (let iter ((n n) (prev 1) (curr 0))
    (if (= n 0) curr
        (iter (- n 1) curr (+ prev curr)))))
```

> Compare o tempo de execução de `fib` e `fibo` para n = 45. <br/>
> Aproveite para tentar transformar o procedimento `fac` em uma iteração linear.

Alguns processos lineares podem ser otimizados ainda mais se explorarmos alguma propriedade que permita realizar, de uma vez, uma certa operação equivalente a múltiplas iterações.

```scheme
;; recursao O(2^(n-1)) para base 2
;; 2^n = 2 * 2^(n-1) = 2^(n-1) + 2^(n-1)
(define (2^ n)
  (cond ((= n 0) 1)
        ((= n 1) 2)
        (else (+ (2^ (- n 1))
                 (2^ (- n 1))))))

;; recursao O(n)
;; b^n = b * b^(n-1)
(define (^ b n)
  (if (= n 0) 1 (* b (^ b (- n 1)))))

;; iteracao O(n)
;; b^n = b * b^(n-1)
(define (^ b n)
  (let iter ((n n) (prod 1))
    (if (= n 0) prod
        (iter (- n 1) (* b prod)))))

;; recursao O(log2(n))
;; b^n = (b^(n/2))^2
(define (^ b n)
  (cond ((= n 0) 1)
        ((even? n) (square (^ b (halve n))))
        (else (* b (^ b (- n 1))))))

;; iteracao O(log2(n))
;; b^n = (b^2)^(n/2)
(define (^ b n)
  (define (iter b n prod)
    (cond ((= n 0) prod)
          ((even? n) (iter (square b) (halve n) prod))
          (else (iter b (- n 1) (* b prod)))))
  (if (< n 0)
      (iter (/ 1 b) (- n) 1)
      (iter b n 1)))

```

```scheme
;; PS:
(define (halve x) (ash x -1))
(define (square x) (* x x))
```

> Tente aplicar a mesma técnica para computar a multiplicação de dois inteiros `b` e `n` em tempo logarítmico.
> O procedimento resultante deve se assemelhar ao algoritmo de [multiplicação egípcia/russa](https://en.wikipedia.org/wiki/Ancient_Egyptian_multiplication).
