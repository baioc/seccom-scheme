## *Memoization* com *Closures*

Se analisarmos a primeira definição do procedimento `fib`, percebemos que seu principal problema é a computação de resultados que já foram vistos e calculados anteriormente.
Uma forma de contornar esse problema seria fazer com que o processo se lembre desses resultados parciais em um tipo de memória, evitando assim uma computação que pode vir a ser custosa.
Essa técnica de [programação dinâmica](https://en.wikipedia.org/wiki/Dynamic_programming) se chama [*memoization*](https://en.wikipedia.org/wiki/Memoization).

```scheme
(define (fib n)
  (display "Computing Fib of ") (display n) (newline)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (memo-fib (- n 1))
                 (memo-fib (- n 2))))))

(define cache (make-table))

(define (memo-fib n)
  (let ((hit? (lookup cache n)))
    ;; hit or miss
    (or hit?
        (let ((result (fib n)))
          (insert! cache n result)
          result))))

;; (clear-table! cache)
```

> Scheme adota a convenção de sinalizar procedimentos que geram efeitos colaterais com o sufixo `!`. <br/>

```scheme
(define (make-table)
  (cons '*table* '()))

(define (lookup table key)
  (let ((record (assoc key (cdr table))))
    (cond (record => cdr)
          (else #f))))

(define (insert! table key value)
  (cond ((assoc key (cdr table)) => (lambda (record) (set-cdr! record value)))
        (else (set-cdr! table
                        (cons (cons key value) (cdr table)))))
  'ok)

(define (clear-table! table)
  (set-cdr! table '())
  'ok)

; (define (assoc k a-list)
;   (cond ((null? a-list) #f)
;         ((equal? k (caar a-list)) (car a-list))
;         (else (assoc k (cdr a-list)))))
```

> Tomando a tabela apresentada como exemplo, tente implementar uma pilha (LIFO) com estado mutável.
> Depois, explique como fazer o mesmo através de programação puramente funcional.

A estrutura de dados implementada é um exemplo de sistema cujos resultados dependem não somente das entradas dadas, mas também do seu estado interno.

Perceba que a única coisa atrelada à sequência de Fibonacci no corpo de `memo-fib` é a chamada da função `fib`.
Podemos generalizar essa ideia em uma função de alta ordem que retorna um *closure* contendo sua própria "memória".

```scheme
(define (memoize proc)
  (let ((cache (make-table)))
    (define (delegate . args)
      (let ((hit (lookup cache args)))
        (or hit
            (let ((result (apply proc args)))
              (insert! cache args result)
              result))))
    delegate))
```

```scheme
(define memo-fib (memoize
  (lambda (n)
    (display "Computing Fib of ") (display n) (newline)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (memo-fib (- n 1))
                   (memo-fib (- n 2))))))))

(define memo-sqrt (memoize sqrt))

;; <etc>
```

Apesar de termos melhorado a performance do procedimento antes exponencial para linear, o fizemos em troca de memória da máquina.
Além disso, o sistema de busca de resultados armazenados não é muito eficiente: se na avaliação de ``(+ (memo-fib (- n 1)) (memo-fib (- n 2)))`` as entradas na *cache* estiverem em ordem crescente, seria necessário varrer a lista do início até encontrar a chave ``(- n 1)`` e depois recomeçar o processo indo do ínicio da tabela até a entrada ``(- n 2)``.

Por causa de situações como estas, implementações sérias de Scheme provêm formas de utilizar estruturas de dados mais eficientes projetadas em outras linguagens.
Em Guile, por exemplo, poderíamos utilizar uma *hashtable* para chegar o mais próximo possível de calcular um dado número de Fibonacci em tempo constante (supondo que já tenha sido pré-computado).

```scheme
;; GUILE
(define make-table make-hash-table)
(define lookup hash-ref)
(define insert! hash-set!)
```
