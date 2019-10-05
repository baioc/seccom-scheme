### Diferenciação Analítica

Um outra maneira de utilizar Scheme como metalinguagem circular é na obtenção da derivada analítica de uma função.
Nesse caso, não é o procedimento que computa a função que é passado ao gerador (como era o caso na diferenciação numérica utilizada no método de Newton de uma seção anterior), mas sim uma descrição de como seria o código desse procedimento na linguagem Scheme.

```scheme
(define (differentiate var sexpr)
  (cond ((number? sexpr) 0)
        ((variable? sexpr)
         (if (same-variable? sexpr var) 1 0))
        ((sum? sexpr)
         (make-sum (differentiate var (augend sexpr))
                   (differentiate var (addend sexpr))))
        ((product? sexpr)
         (make-sum
           (make-product (multiplier sexpr)
                         (differentiate var (multiplicand sexpr)))
           (make-product (differentiate var (multiplier sexpr))
                         (multiplicand sexpr))))
        (else
         (error "unknown expression type -- DERIV" sexpr))))

(define (make-derivative var sexpr)
  (let ((deriv (differentiate var sexpr)))
    (eval
      `(lambda (,var) ,deriv)
      (interaction-environment))))


(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum augend addend)
  (cond ((and (number? addend) (number? augend)) (+ augend addend))
        ((and (number? augend) (= augend 0)) addend)
        ((and (number? addend) (= addend 0)) augend)
        (else (list '+ augend addend))))

(define (sum? x)
  (and (list? x) (eq? (car x) '+)))

(define (augend s)
  (cadr s))

(define (addend s)
  (caddr s))

(define (make-product multiplier multiplicand)
  (define (=number? sexpr num)
    (and (number? sexpr) (= sexpr num)))
  (cond ((or (=number? multiplier 0) (=number? multiplicand 0))
          0)
        ((and (number? multiplier) (number? multiplicand))
          (* multiplier multiplicand))
        ((=number? multiplier 1) multiplicand)
        ((=number? multiplicand 1) multiplier)
        (else (list '* multiplier multiplicand))))

(define (product? x)
  (and (list? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (caddr p))
```

> Adicione mais algumas [regras de diferenciação](https://en.wikipedia.org/wiki/Differentiation_rules) no programa acima.

A partir do código dado temos um sistema capaz de gerar procedimentos que computam polinômios arbitrários (de uma variável independente), assim como suas derivadas (incluindo derivadas parciais).

```scheme
(define coefs '(-1 -1 1))

(define p
  (apply horner
         (list 'x coefs))) ;; p = x^2 - x - 1

(define f
  (apply make-polynomial
         (cons 'x coefs))) ;; f(x) = p(x)

(define df
  (make-derivative 'x p))  ;; df(x) = p'(x) = 2x - 1
```

> Pense sobre como seria possível expandir esse programa para permitir a geração de polinômios multivariados.
> Você pode discutir suas ideias na [seção de comentários](./discussion.md) desse site.
