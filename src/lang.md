## Sobre Linguagens

> Qualquer notação usada para dar instruções pode ser considerada uma linguagem de programação ([Schmidt][denSem]).

Talvez pela familiaridade com a área e a base formal já existente, a maioria das linguagens de programação de uso geral (incluindo Scheme) se baseia em computar funções matemáticas através de expressões.
O mecanismo principal se baseia na "coincidência" de que a **expressão** que descreve o **valor** de uma função pode ser **interpretada** como um **procedimento** para **computar** aquele valor ([Abelson e Sussman, 1968][sicpLogic]).

Por exemplo, tendo uma função polinomial expressa em "matematiquês":

\\[ x^2 - x - 1 \\]

Podemos expressá-la com caracteres padrão em alguma linguagem (por acaso a notação abaixo é código válido de Octave, provavelmente o sendo em mais uma série de outras linguagens):

```m
x ^ 2 - x - 1
```

Um polinômio equivalente na [notação pré-fixada](https://en.wikipedia.org/wiki/Polish_notation) de Lisp seria

```scheme
(+ (^ x 2) (- x) -1)
```

[sicpLogic]: https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-29.html
[denSem]: https://www.cs.colorado.edu/~bec/courses/csci5535/reading/densem.pdf

### Sintaxe e Semântica

> Programas devem ser escritos para que pessoas possam os ler, e apenas incidentalmente para que máquinas os executem ([Abelson e Sussman, 1968][sicpPrograms]).

A notação das expressões de uma linguagem específica é dita a sua sintaxe; e mesmo que esse tópico não nos interesse no escopo deste minicurso, começamos falando sobre sintaxe pois apenas expressões sintaticamente corretas possuem semântica, ou seja, significado, associado a elas.

Mais importante ainda, falamos da sintaxe de Scheme para destacar seu minimalismo: ela é composta essencialmente pelas chamadas **expressões simbólicas** ([*sexprs*](https://en.wikipedia.org/wiki/S-expression)), cada uma delas caracterizando uma árvore binária, podendo ser:

1. Um átomo; ou
2. Uma expressão na forma ``(head . tail)`` onde `head` e `tail` são *sexprs*.

[sicpPrograms]: https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-7.html

### Pragmática

Aos prospectivos utilizadores de uma linguagem de programação interessa mais um outro elemento: a **pragmática**, ou seja, os aspectos práticos da linguagem que a tornam útil para alcançar um conjunto de possíveis objetivos ([Cameron][pragmatics]).

Neste minicurso será dado enfoque especial à aplicações matemáticas de Scheme como linguagem de programação funcional, onde a principal forma de **abstração** é encontrada na manipulação de funções.

[pragmatics]: https://www2.cs.sfu.ca/~cameron/Teaching/383/syn-sem-prag-meta.html

### Metalinguagem

Por último, mas não menos importante, temos a ferramenta com a qual falamos sobre uma linguagem, descrevemos suas características e discutimos suas aplicações: a **metalinguagem** que lhe cabe.

Mais adiante veremos que é possível empregar Scheme como uma linguagem de programação de uso geral que não é especialmente adequada a resolver nenhuma classe de problemas; em vez disso, seu maior potencial está na capacidade de construir e embarcar em si mesma a linguagem mais apta para fazê-lo através de abstrações metalinguísticas ([Abelson e Sussman][sicpMeta]).

[sicpMeta]: https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-25.html
