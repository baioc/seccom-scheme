## Introdução à Scheme

### Programas

> Os atos pelos quais a mente exerce seu poder sobre as idéias são principalmente três:
> (1) Combinando várias idéias simples em uma composta e originando assim todas as idéias complexas;
> (2) Reunindo duas idéias, simples ou complexas, a fim de ter uma visão delas, sem, contudo, unificá-las numa, obtendo por este meio todas as suas noções das relações;
> (3) Separando-as de todas as outras idéias que lhes integram, no processo chamado abstração: deste modo a mente forma todas as suas idéias gerais.
> ([Locke, 1689][lockeIdeas])

Toda a linguagem de programação que se preze possui três mecanismos básicos com os quais expressa programas:

1. **Entidades primitivas**: os objetos mais simples dos quais fala a linguagem.
2. **Meios de combinação**: a construção de elementos compostos através de outros mais simples.
3. **Meios de abstração**: pelos quais manipulamos elementos independentemente da sua complexidade.

([Abelson e Sussman][sicpElements]).

[lockeIdeas]: https://oll.libertyfund.org/titles/locke-the-works-vol-1-an-essay-concerning-human-understanding-part-1
[sicpElements]: https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-10.html

### Interpretação

Para testar os exemplos nas subseções a seguir, utilize alguma implementação de Scheme:

- [Chez](https://cisco.github.io/ChezScheme/) - **Ótimo REPL**, possui um dos interpretadores mais rápidos e uma versão adicional minimalista para distribuição de programas interpretados.
- [Bigloo](http://www-sop.inria.fr/mimosa/fp/Bigloo/) - Um exemplo de **transpilador Scheme->C** que permite gerar binários executáveis e altamente portáveis.
- [Guile](https://www.gnu.org/software/guile/) - É a **linguagem de scripting oficial do GNU Project**, sendo utilizada como linguagem de extensão e configuração do [sistema operacional funcional GuixSD](https://guix.gnu.org/).
- [Racket](https://racket-lang.org/) - Tem uma **IDE feita para Scheme, o Dr. Racket**; oferecendo, além da linguagem padrão, mais um grande conjunto de "dialetos".
- [Outros](https://ecraven.github.io/r7rs-benchmarks/) - Procure implementações com comunidade ativa e que se adequem aos padrões [*Revised^n Report on the Algorithmic Language Scheme* (RnRS)](https://en.wikipedia.org/wiki/Scheme_(programming_language)#Review_of_standard_forms_and_procedures). Se você deseja alguma *feature* específica, confira se a sua implementação já a oferece através de algum [*Scheme Request For Implementation* (SRFI)](https://srfi.schemers.org/).

Saiba consultar a [documentação](https://docs.racket-lang.org/) da sua implementação. <br/>
Na dúvida, [Dybvig](https://scheme.com/tspl4/) e [Sitaram](https://ds26gte.github.io/tyscheme/) podem ajudar.

### Estrutura ([exemplos](https://docs.racket-lang.org/racket-cheat/index.html))

#### Primitivas

Alguns exemplos de entidades primitivas em Scheme:

```scheme
; numeros
42
0.03
12e-4
67+89i
12/40

; booleanos
#t
#f

; funcoes primitivas
+
-
odd?
even?
car
cdr

; simbolos
'a
'λ
'scheme

; caracteres
#\c
#\f
```

#### Combinações

Vejamos os meios de combinação presentes em Scheme:

```scheme
; pares
(cons 'head 'tail)
(car (cons 1 2))
(cdr (cons 1 2))

; listas
'()
(list 1 2 3)
(cdr '(a b c))

; funcoes anonimas
(lambda (x) x)
(lambda (a b) (+ a b))

; strings
"Hello, Scheme World!"

; vetores
#(1 2 3)
```

##### Dados *vs* Código

Listas são encadeamentos de pares (como uma corrente) terminadas em `'()`.
Assim, os dados processados em Scheme possuem exatamente a mesma forma que seu código.
Essa propriedade se chama [**Homoiconicidade**](https://en.wikipedia.org/wiki/Homoiconicity).

#### Abstrações

São alguns meios de abstração de Scheme:

```scheme
; nomes temporarios
(let ((x 7)
      (y 8))
  (+ x y))

(let* ((a 2)
       (b (+ a 1))
       (c (* a b)))
  (+ a b c))

; definicoes
(define x 5)

(define (identity x) x)

(define list
  (lambda (first . rest) (cons first rest)))

(define (foo x)
  (define (even? n)
    (display "even? ") (display n) (newline)
    (if (= n 0)
        #t
        (odd? (- n 1))))

  (define (odd? n)
    (display "odd? ") (display n) (newline)
    (if (= n 0)
        #f
        (even? (- n 1))))

  (cond ((even? x) "EVEN")
        ((odd? x) "ODD")
        (else "????")))

; mix
(let 2^ ((n 10)
         (acc 1))
  (if (= n 0) acc
      (2^ (- n 1) (* acc 2))))

(let ((a 1))
  (define (foo x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (display (foo 10)) (newline)
  a)
```

> Sim, `foo` tem vários problemas.
> Tente corrigi-los.
>
> Procure escrever em Scheme a definição de um procedimento que calcula em um ponto `x` o valor do polinômio \\( x^2 - x -1 \\).
>
> Em seguida, implemente um procedimento `(list-ref lst n)` que retorna o n-ésimo elemento de uma lista.
