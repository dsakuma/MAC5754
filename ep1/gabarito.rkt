#lang plai

;
; Funções não têm mais nome, serão chamadas de lamC (em homenagem ao λ)
;

; Expressões básicas
(define-type ExprC
  [numC    (n  number?)]
  [idC     (s  symbol?)]
  [plusC   (l  ExprC?) (r  ExprC?)]
  [multC   (l  ExprC?) (r  ExprC?)]
  [lamC    (arg  symbol?) (body  ExprC?)] ; nomes não são mais necessários
  [appC    (fun  ExprC?) (arg  ExprC?)]
  [ifC     (condition  ExprC?) (yes  ExprC?) (no  ExprC?)]
  [letRecC (s  symbol?) (arg  ExprC?) (body  ExprC?)]
  [quoteC  (s  symbol?)]
  [loadC   (e  ExprC?)]
  )

; Expressões açucaradas
(define-type ExprS
  [numS    (n  number?)]
  [idS     (s  symbol?)]
  [lamS    (arg  symbol?) (body  ExprS?)] ; muda de acordo
  [appS    (fun  ExprS?) (arg  ExprS?)]
  [plusS   (l  ExprS?) (r  ExprS?)]
  [bminusS (l  ExprS?) (r  ExprS?)]
  [uminusS (e  ExprS?)]
  [multS   (l  ExprS?) (r  ExprS?)]
  [ifS     (c  ExprS?) (s  ExprS?) (n  ExprS?)]
  [letS    (s  symbol?) (arg  ExprS?) (body  ExprS?)]
  [let*S   (s1  symbol?) (arg1  ExprS?) (s2  symbol?) (arg2  ExprS?) (body  ExprS?)]
  [letRecS (s  symbol?) (arg  ExprS?) (body  ExprS?)]
  [quoteS  (s  symbol?)]
  [loadS   (e  ExprS?)]
  )

; Retirando o açúcar
(define (desugar as); ExprS => ExprC
  (type-case ExprS as
    [numS    (n) (numC n)]
    [idS     (s) (idC s)]
    [lamS    (a b)  (lamC a (desugar b))]
    [appS    (fun arg) (appC (desugar fun) (desugar arg))]
    [plusS   (l r) (plusC (desugar l) (desugar r))]
    [multS   (l r) (multC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)   (multC (numC -1) (desugar e))]
    [ifS     (c s n) (ifC (desugar c) (desugar s) (desugar n))]
    [letS    (s a b) (appC (lamC s (desugar b)) (desugar a))]
    [let*S   (s1 a1 s2 a2 b) (appC (lamC s1 (appC (lamC s2 (desugar b)) (desugar a2))) (desugar a1))]
    [letRecS (s a b) (letRecC s (desugar a) (desugar b))]
    [quoteS  (s) (quoteC s)]
    [loadS   (e) (loadC (desugar e))]
    ))

;
; Closures não têm mais nome, mas precisam de Environment
;

; Símbolos devem se associar a um Value
(define-type Binding
      [bind (name  symbol?) (val  Value?)])

; A lista de associações é o Environment
; (define-type-alias Env (listof Binding))
(define mt-env empty)    ; Tente pronunciar "mt" em inglês e compare com "empty"
(define extend-env cons) ; Por sorte, cons faz exatamente o que queremos para estender o env
(define-type Value
  [numV  (n  number?)]
  [symV  (s  symbol?)]
  [closV (arg  symbol?) (body  ExprC?) (env  list?)])

; Novos operadores
(define (num+ l r); Value x Value => Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (+ (numV-n l) (numV-n r)))]
        [else
             (error 'num+ "Um dos argumentos não é número")]))

(define (num* l r); Value x Value => Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (* (numV-n l) (numV-n r)))]
        [else
             (error 'num* "Um dos argumentos não é número")]))

; Interpretador
(define (interp arg env); ExprC x Env => Value
  (type-case ExprC arg
    [numC  (num) (numV num)]
    [idC   (sym) (lookup sym env)]
    [lamC  (sym body) (closV sym body env)] ; definição de função captura o environment

    [appC  (fun arg)
             (local ([define fun-value (interp fun env)]) ; fun-value descreve melhor a ideia; fun-value é uma closV
               (interp (closV-body fun-value); body da closV fun-value
                       (extend-env
                           (bind (closV-arg fun-value) (interp arg env)); gg
                           (closV-env fun-value) ; environment da closV fun-value
                       )))]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [ifC   (c s n) (if (zero? (numV-n (interp c env))) (interp n env) (interp s env))]
    [letRecC (sym fun body)
               (local ([define dummy-clos
                        (closV sym (idC sym) mt-env)] ; cria um fechamento que recebe fac e retorna idC fac    
                       [define tmp-env
                         (extend-env (bind sym dummy-clos) env)] ;cria um ambiente com fac apontando para um fechamento que recebe fac e retorna idC fac
                       [define fun-clos
                         (interp fun tmp-env)] ;cria um fechamento capturando o ambiente tmp-env 
                       [define fixed-env
                         (extend-env (bind sym fun-clos) env)]) ; cria um ambiente onde fac aponta para o fechamento fun-clos
                      ;(println dummy-clos)          
                      ;(println tmp-env) 
                      ;(println fun) 
                      ;(println fun-clos)
                      ;(println fixed-env)
                      (begin
                        (set-closV-env! fun-clos fixed-env)
                        (println fun-clos)
                        ;(println fixed-env)
                        (interp body fixed-env)))]
    [quoteC (sym) (symV sym)]
    [loadC (expr)
           (letrec ([interp-all
                      (lambda (fh)
                        (let ([line (read fh)])
                          (unless (eof-object? line)
                            (begin (println
                                     (interp
                                       (desugar
                                         (parse line))
                                       mt-env))
                                   (interp-all fh)))))])
             (interp-all (open-input-file
                           (string->path
                             (symbol->string
                               (symV-s (interp expr env)))))))
           ]
    ))

; Lookup para procurar símbolos no Environment
(define (lookup for env); [for : symbol] [env : Env]) => Value
       (cond
            [(empty? env) (error 'lookup (string-append (symbol->string for) " não foi encontrado"))] ; livre (não definida)
            [else (cond
                  [(symbol=? for (bind-name (first env)))   ; achou!
                                 (bind-val (first env))]
                  [else (lookup for (rest env))])]))        ; vê no resto

; Parser
(define (parse s); [s : s-expression] => ExprS
  (cond
    [(number? s) (numS s)]
    [(symbol? s) (idS  s)] ; Pode ser um símbolo livre nas definições de função
    [(list? s)
     (let ([sl s])
       (case  (first sl)
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(~) (uminusS (parse (second sl)))]
         [(func) (lamS (second sl) (parse (third sl)))]
         [(lambda) (lamS (second sl) (parse (third sl)))]
         [(call) (appS (parse (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(let) (letS (first (first (second sl))) (parse (second (first (second sl))))
                      (parse (third sl)))]
         [(let*) (let*S (first (first (second sl))) (parse (second (first (second sl))))
                        (first (second (second sl))) (parse (second (second (second sl))))
                        (parse (third sl)))]
         [(letrec) (letRecS (first (first (second sl))) (parse (second (first (second sl))))
                            (parse (third sl)))]
         [(quote) (quoteS (second sl))]
         [(load) (loadS (parse (second sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

; Facilitador
(define (interpS s) (interp (desugar (parse s)) mt-env))

; Testes
(test (interpS '(letrec ([fac (lambda n (if n (* n (call fac (- n 1))) 1))])
                  (call fac 5)))
      (numV 120))
