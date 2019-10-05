## Recursão Linear

Vejamos o exemplo clássico da função recursiva que calcula o fatorial de um número:

```scheme
(define (fac n)
  (if (= n 0) 1
      (* n (fac (- n 1)))))
```

Como o procedimento `fac` é unidirecional e não gera efeitos colaterais - uma **função pura** - podemos computar o processo recursivo através de um modelo simples: basta substituir a definição de `fac` sempre que encontrarmos uma chamada, trocando também o argumento formal `n` no esqueleto do procedimento pelo valor passado na evocação.
No exemplo abaixo já foi tomado o branch correto na expressão condicional.

```scheme
         (fac 4)
      (* 4 (fac 3))
    (* 4 (* 3 (fac 2)))
  (* 4 (* 3 (* 2 (fac 1))))
(* 4 (* 3 (* 2 (* 1 (fac 0)))))
  (* 4 (* 3 (* 2 (* 1 1))))
    (* 4 (* 3 (* 2 1)))
      (* 4 (* 3 2))
        (* 4 6)
          24
```

Destaca-se que a computação deve armazenar no contexto do corpo da função `fac` a associação do valor da variável `n`.
Isso acontece pois a multiplicação em `(* n (fac (- n 1)))` é avaliada somente após o retorno da chamada recursiva.

Assim, o processo transcorre tanto em tempo linear (altura da árvore) como em espaço linear (tamanho da pilha de chamadas) em relação à entrada `n`.
