# Sistemas Computacionais

Até então temos visto programas que operam sobre dados em uma grande composição de funções, onde a saída de um procedimento alimenta a entrada de outro e assim por diante.
O **fluxo de dados** é sempre unidirecional e **depende somente do arranjo dos procedimentos e das entradas iniciais**, caracterizando programas **puramente funcionais**.
Além disso, o modelo aplicado para computar o resultado de um programa é muito simples, bastando substituir aplicações de procedimentos por sua definição instanciada com os argumentos fornecidos.

Alguns problemas exigem um **fluxo de dados** diferente.
Gostaríamos de introduzir maneiras de alterar o comportamento do programa de formas diferentes **dependendo do instante em que o sistema se encontra**.

### Perceba que, **desde o início da oficina até este ponto, jamais fez-se necessário utilizar conceitos como variáveis, atribuições ou estado mutável**; é o que faremos agora.

> Na medida em que atribuições e ações de mudança são introduzidas na programação, acabamos de nos sujeitar a todos os problemas terríveis que vêm assolando filósofos por milhares de anos. ([Sussman, 1968](https://www.youtube.com/watch?v=dO1aqPBJCPg&list=PLE18841CABEA24090&index=9))

Em Scheme, a ferramenta para tal é `set!` e seus derivados[[2]](https://www.youtube.com/watch?v=yedzRWhi-9E&list=PLE18841CABEA24090&index=10):

```scheme
(define count 10)
(define (count-down)
  (set! count (- count 1))
  count)

(define a (cons 1 2))
(define b (cons a a))
(define a (cons 3 2))
b ; = ?

(define a (cons 1 2))
(define b (cons a a))
(set-car! a 3)
b ; = ?
```

> Explique o bug no procedimento `set-fact`.

```scheme
(define (fact n)
  (let loop ((i 1) (p 1))
    (if (> i n) p
        (loop (+ i 1) (* p i)))))

(define (set-fact n)
  (let ((i 1) (p 1))
    (let loop ()
      (if (> i n) p
          (begin
            (set! i (+ i 1))
            (set! p (* p i))
            (loop))))))
```

{{#include memo.md}}

{{#include constr.md}}

{{#include inf.md}}
