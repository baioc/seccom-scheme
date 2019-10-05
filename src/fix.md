## Pontos Fixos

Vejamos agora um procedimento que não define um algoritmo para computar um valor, mas sim um método numérico para aproximar a raíz quadrada de um número:

```scheme
;; metodo babilonico
(define (sqrt x)
  (define (try guess)
    (if (good-enough? guess) guess
        (try (improve guess))))

  (define (improve guess)
    (average guess (/ x guess)))

  (define (good-enough? guess)
    (< (abs (- (square guess) x)) tolerance))

  (try 1.0))
```

```scheme
;; PS:
(define tolerance 1e-15)
(define (average a b) (/ (+ a b) 2))
(define (square x) (* x x))
```

Com um procedimento semelhante é possível aproximar uma solução da [equação transcedental](https://en.wikipedia.org/wiki/Transcendental_equation) \\( cos(x) = x \\).

```scheme
(define (fixcos)
  (define (retry old new)
    (if (approx? new old) new
        (retry new (cos new))))

  (define (approx? a b)
    (< (abs (- a b)) tolerance))

  (retry 0.0 (cos 0.0)))

(define x (fixcos)) ;; solucao
```

> Um método "manual" para achar esse valor envolve uma calculadora científica e a seguinte sequência de botões:
>
> ```plaintext
> [1] [=] [COS] [ANS] [=] [=] [=] ...
> ```
>
> Tendo isso em mente, tente implementar um procedimento `(fixpoint f x)` que generalize ambos os métodos apresentados.

Nota-se uma ideia em comum: procuramos um **ponto fixo** ou ponto invariante k de uma função f(x) tal que \\( f(k) = k \\).
Assim, supondo um valor intermediário c onde \\( f(c) \approx k \\), temos que \\( f(f(...f(c)...)) = f^n(f(c)) = f^n(k) = k \\), onde aplicamos f até que não haja mais variação (com uma certa tolerância) no resultado.

```scheme
(define (fixpoint f x . opts-tol)
  (let* ((tolerance (maybe-car opts-tol 1e-9))
         (approx? (lambda (a b) (< (abs (- a b)) tolerance))))
    (let try ((old x) (new (f x)))
      (if (approx? old new) new
          (try new (f new))))))
```

```scheme
(fixpoint cos 0)

(define (phi-rat tol)
  (fixpoint
    (lambda (rat)
      (let ((fcurr (numerator rat))
            (fprev (denominator rat)))
        (/ (+ fcurr fprev) fcurr)))
    1/1
    tol))
(define phi (exact->inexact (phi-rat 1e-15))) ;; => 102334155/63245986 = fib(40)/fib(39)

(define (average-damp f)
  (lambda (x) (average (f x) x)))

(define (sqrt x)
  (fixpoint (average-damp (lambda (y) (/ x y))) 1.0))

(define (root f x . opts-tol) ;; metodo de Newton
  (let* ((dx (maybe-car opts-tol 1e-8))
         (df (deriv f dx)))
    (fixpoint (lambda (x) (- x (/ (f x) (df x)))) x dx)))

(define (deriv f dx)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (sqrt x)
  (root (lambda (y) (- x (square y))) 1.0))

(define pi (root sin 3))

(define phi (root (lambda (x) (+ (square x) (- x) -1)) 1.0 1e-11))
```

O [Cálculo Lambda](https://en.wikipedia.org/wiki/Lambda_calculus) de Alonzo Church generaliza a noção de ponto fixo para funções, possibilitando a computação de procedimentos recursivos.
Vejamos um exemplo partindo do algoritmo fatorial:

```scheme
(define (fac n)
  (define f fac)
  (if (= n 0) 1 (* n (f (- n 1)))))

(define (facto n)
  (define f facto)
  (define (aux n)
    (if (= n 0) 1 (* n (f (- n 1)))))
  (aux n))

(define (factor n)
  (define (aux f n)
    (if (= n 0) 1 (* n (f (- n 1)))))
  (aux factor n))

(define (factori n)
  (define (aux f)
    (lambda (n)
      (if (= n 0) 1 (* n (f (- n 1))))))
  ((aux factori) n))

(define (factoria n)
  (define (aux f)
    (lambda (n)
      (if (= n 0) 1 (* n (f (- n 1))))))
  (define (rec f)
    (lambda (n)
      ((aux (f f)) n)))
  (let ((fact (rec rec)))
    (fact n)))
```

Perceba que a estrutura e o uso do procedimento `rec` podem ser facilmente generalizados na função de alta ordem ilustrada abaixo.
Esse procedimento também é conhecido como [Operador Y](https://en.wikipedia.org/wiki/Fixed-point_combinator#Y_combinator).

```scheme
(define (fix f)
  (define (g x)
    (lambda (arg)
      ((f (x x)) arg)))
  (g g))
```

```scheme
(define fact
  (fix (lambda (f)
         (lambda (n)
           (if (= n 0) 1 (* n (f (- n 1))))))))
```

Imagine o processo como uma iteração sobre uma função `f` que inicialmente não sabe calcular fatoriais: a cada passo `f` é substituída por uma versão melhorada dela mesma [[1]](https://en.wikipedia.org/wiki/Denotational_semantics).
Podemos representar o estado atual de `f` como um conjunto de duplas `(x,y)`, onde `f` associa uma entrada `x` à saída `y`.

```plaintext
Fe(x) = {}
F0(x) = { (0,1) }
F1(x) = { (0,1), (1,1) }
F2(x) = { (0,1), (1,1), (2,2) }
F3(x) = { (0,1), (1,1), (2,2), (3,6) }
F4(x) = { (0,1), (1,1), (2,2), (3,6), (4,24) }
F5(x) = { (0,1), (1,1), (2,2), (3,6), (4,24), (5,120) }
...
Fn(x) = fact(x), para x <= n
```

Na prática, basta aplicar o modelo de substituição para computar a função recursivamente:

```scheme
(define curryed-fac
  (lambda (f)
    (lambda (n) (if (= n 0) 1 (* n (f (- n 1)))))))
...
(define fact (fix curryed-fac))
;; substituindo fix(f=curryed-fac)
(define fact
  (define (g x)
    (lambda (arg) ((curryed-fac (x x)) arg)))
  (g g))
;; substituindo g(x=g)
(define fact
  (lambda (arg) ((curryed-fac (g g)) arg)))
...
(fact 5)
;; substituindo fact(arg=5)
((curryed-fact (g g)) 5)
;; substituindo g(x=g)
((curryed-fact (lambda (arg) ((curryed-fac (g g)) arg))) 5)
;; perceba que o lambda eh exatamente a definicao de fact
((curryed-fact fact) 5)
;; substituindo curryed-fact(f=fact)
((lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))) 5)
;; substituindo lambda(n=5)
(* 5 (fact 4))
...
```
