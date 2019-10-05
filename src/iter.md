## Recursão Iterativa: *Tail-Recursion*

Um outro exemplo de procedimento recursivo é o algoritmo de Euclides:

```scheme
(define (gcd a b)
  (if (= b 0) a
      (gcd b (modulo a b))))
```

Compare o processo gerado com o anterior.

```scheme
(gcd 1071 462)
(gcd 462 147)
(gcd 147 21)
(gcd 21 0)
21
```

Perceba que no caso do `gcd`, a computação não exige o armazenamento do contexto antes das chamadas recursivas: **toda a informação necessária encontra-se nos argumentos da função**.

Ambos os procedimentos denotam **recursões lineares**, ou seja, onde a invocação da função ocorre uma única vez durante a sua avaliação.
Entretanto, enquanto `fac` toma uma quantidade de memória proporcional ao argumento de entrada, o processo descrito por `gcd` exige espaço constante e portanto constitui ainda uma **iteração linear**.

O que diferencia os dois procedimentos é o fato de que a última expressão no corpo de `gcd` é a invocação de uma função.
Quando isso acontece dizemos que a função possui uma [*tail-call*](https://en.wikipedia.org/wiki/Tail_call) (chamada de cauda ou terminal) e quando a função evocada é recursiva, dizemos que ela é ***tail-recursive***.
Esse fenômeno possibilida a aplicação da chamada *Tail Call Optimization*, que elimina a necessidade de ocupar espaço na pilha de execução do programa.

A especificação da linguagem Scheme exige que implementações sejam [*properly tail-recursive*](http://people.csail.mit.edu/jaffer/r5rs/Proper-tail-recursion.html), o que significa que o número de execuções de funções em *tail-calls* sequenciais é **ilimitado**.
