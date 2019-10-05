## *Lazy Streams* no Infinito

Tendo observado outros tipos de sistemas, voltemos aos unidirecionais com o intuito de explorar esse paradigma de fluxo de dados: se um programa consiste no encadeamento de funções puras, não importa o momento em que um valor intermediário é computado, desde que quando o resultado final seja requisitado ele se faça presente.
Sistemas assim podem se dar ao luxo de serem "preguiçosos", adiando ao máximo o instante em que realizarão uma computação.

> Computadores, assim como pessoas, tentam adiar ao máximo possível a ocorrência de eventos desagradáveis.
> ([Tanenbaum, 2011][tanembaumLazy])

[tanembaumLazy]: https://en.wikipedia.org/wiki/Modern_Operating_Systems

A seguir implementaremos um tipo de lista que segue essa ideia de [*lazy evaluation*](https://en.wikipedia.org/wiki/Lazy_evaluation).
Os principais procedimentos pelos quais essas "listas preguiçosas" serão manipuladas estão descritos abaixo.

```scheme
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
```

> Construa um procedimento `(stream-range lo hi)` que gera uma stream com todos os inteiros entre `lo` e `hi`.

As primitivas que constroem e acessam as *streams* são responsáveis por diferenciá-las de listas normais.

```scheme
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

;; delay
(define-syntax lazy
  (syntax-rules ()
    ((lazy expr)
      (memo-proc (lambda () expr)))))

;; force
(define (thunk p) (p))

(define-syntax stream
  (syntax-rules ()
    ((stream x y)
      (cons x (lazy y)))))

(define (head s) (car s))
(define (tail s) (thunk (cdr s)))
(define (empty? s) (null? s))
(define empty-stream '())
```

As [macros](http://ds26gte.github.io/tyscheme/index-Z-H-10.html) `stream` e `lazy` definem transformações que ocorrem no código antes da sua interpretação.
Assim, uma *stream* consiste num par onde a computação do segundo elemento foi postergada, deixando apenas uma promessa de que estará lá quando for necessária.
Se um elemento de uma *stream* nunca for acessado, a promessa não precisa ser cumprida e nenhum processamento é efetuado.

Note que esse mecanismo permite representar sequências infinitas: todos os elementos estarão presentes, mas só existirão de fato quando alguém requisitar.

```scheme
(define (ints-from n)
  (stream n (ints-from (+ n 1))))

(define (sieve seq) ;; crivo de Eratostenes
  (define (divisible? a b)
    (= 0 (remainder a b)))
  (if (empty? seq) seq
      (stream (head seq)
              (sieve (stream-filter (lambda (x) (not (divisible? x (head seq))))
                                    (tail seq))))))

(define primes (sieve (ints-from 2)))
```

Podemos afirmar, portanto, que `primes` é a *stream* de todos os números primos.

O esquema pode ser aplicado também para séries infinitas, por exemplo na [série de Taylor-Maclaurin](https://en.wikipedia.org/wiki/Taylor_series) para o arco tangente

\\[ \arctan x = x - \frac{x^3}{3} + \frac{x^5}{5} - \frac{x^7}{7} + ... \\]

```scheme
(define (arctan-series x n)
  (stream (/ (expt x n) n)
          (stream-map - (arctan-series x (+ n 2)))))
```

> Podemos substituir x=1 para poder encontrar a [fórmula de Madhava](https://en.wikipedia.org/wiki/Madhava_series) para aproximar o valor de \\( \pi \\):
>
> \\[ \arctan 1 = \frac{\pi}{4} = 1 - \frac{1}{3} + \frac{1}{5} - \frac{1}{7} + ... \\]
>
> Tente gerar uma *stream* contendo as aproximações de pi pela fórmula anterior, onde cada n-ésimo elemento equivale à soma parcial de n termos da série.

```scheme
(define (partial-sums s)
  (stream-zip-with + s (stream 0 (partial-sums s))))

(define pi-approximations
  (stream-map (lambda (pi/4) (* 4 pi/4))
              (partial-sums (arctan-series 1.0 1))))
```

> Por fim, vamos obter a sequência de Fibonacci completa em uma *stream* infinita.

```scheme
(define (fibonacci-sequence prev curr)
  (stream prev
          (fibonacci-sequence curr (+ prev curr))))

(define fibs (fibonacci-sequence 0 1))
```

> Reflita sobre a possibilidade de computar elementos de uma *lazy stream* paralelamente com técnicas de programação concorrente.
> Você pode discutir suas ideias na [seção de comentários](./discussion.md) desse site.
