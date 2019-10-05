## Processamento Simbólico

O procedimento `eval` (complementado por `apply`) é a principal ferramenta para abstração metalinguística em Scheme.
Para utilizá-lo, basta fornecer uma expressão simbólica válida e um objeto representando o ambiente computacional onde ela será avaliada; o retorno de `eval` será o valor resultante da interpretação daquela expressão.

```scheme
(define env (scheme-report-environment 5))

'(+ 1 1)              ; => (+ 1 1)
(list '+ 1 1)         ; => (+ 1 1)
(+ 1 1)               ; => 2
(eval '(+ 1 1) env)   ; => 2

(define x 7)
(define y 5)
'(- x y)              ; => (- x y)
(list '- x y)         ; => (- 7 5)
(- x y)               ; => 2
`(- ,x ,y)            ; => (- 7 5)
(eval `(- ,x ,y) env) ; => 2
(eval '(- x y) env)   ; => Unbound variable: x
```

> Com base nos exemplos anteriores, explique como funcionam as citações parciais com os símbolos `` ` `` e `,`

O [método de Horner](https://en.wikipedia.org/wiki/Horner's_method) costuma ser utilizado para calcular o valor de um polinômio de forma a evitar exponenciações; efetua apenas n adições e n multiplicações para um polinômio de grau n.

\\[ Pn(x) = a_0 + a_1 x + a_2 x^2 + ... + a_n x^n \\]
\\[ Pn(x) = a_0 + x (a_1 + x (a_2 + x(...a_n...))) \\]

Vamos aplicar o método de uma maneira um tanto não-convencional, em um algoritmo que "compila" uma lista de coeficientes e gera um procedimento que calcula o polinômio equivalente na forma de Horner:

```scheme
(define (horner x pn)
    (if (null? (cdr pn))
        (car pn)
        `(+ ,(car pn) (* ,x ,(horner x (cdr pn))))))

(define (make-polynomial x first-coef . rest-coefs)
  (let ((polynomial (horner x (cons first-coef rest-coefs))))
    (eval
      (list 'lambda (list x) polynomial)
      (interaction-environment))))
```
