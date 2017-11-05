#lang plai-typed

#|
 | Adding boxes, sequences with 2 expressions, variables and lists
 |#

(define-type ExprC
  [numC    (n : number)]
  [idC     (s : symbol)]
  [plusC   (l : ExprC) (r : ExprC)]
  [multC   (l : ExprC) (r : ExprC)]
  [lamC    (arg : symbol) (body : ExprC)]
  [appC    (fun : ExprC) (arg : ExprC)]
  [ifC     (cond : ExprC) (y : ExprC) (n : ExprC)]
  [seqC    (b1 : ExprC) (b2 : ExprC)]; Executes b1 then b2
  [consC   (car : ExprC) (cdr : ExprC)]; Creates cell with a pair
  [carC    (pair : ExprC)]; Gets 1st element of a pair
  [cdrC    (pair : ExprC)]; Gets 2nd element of a pair
  [equal?C (l : ExprC) (r : ExprC)]
  )


(define-type ExprS
  [numS    (n : number)]
  [idS     (s : symbol)] 
  [lamS    (arg : symbol) (body : ExprS)]
  [appS    (fun : ExprS) (arg : ExprS)] 
  [plusS   (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS   (l : ExprS) (r : ExprS)]
  [ifS     (c : ExprS) (y : ExprS) (n : ExprS)]
  [seqS    (b1 : ExprS) (b2 : ExprS)]
  [consS   (car : ExprS) (cdr : ExprS)]
  [carS    (pair : ExprS)]
  [cdrS    (pair : ExprS)]
  [equal?S  (l : ExprS) (r : ExprS)]
  [letS    (s : symbol) (v : ExprS) (body : ExprS)]
  [let*S   (s1 : symbol) (v1 : ExprS) (s2 : symbol) (v2 : ExprS) (body : ExprS)]
  )


(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
    [numS    (n)        (numC n)]
    [idS     (s)        (idC s)]
    [lamS    (a b)      (lamC a (desugar b))]
    [appS    (fun arg)  (appC (desugar fun) (desugar arg))] 
    [plusS   (l r)      (plusC (desugar l) (desugar r))] 
    [multS   (l r)      (multC (desugar l) (desugar r))]
    [bminusS (l r)      (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)        (multC (numC -1) (desugar e))]
    [ifS     (c y n)    (ifC (desugar c) (desugar y) (desugar n))]
    [seqS    (b1 b2)    (seqC (desugar b1) (desugar b2))]
    [consS   (b1 b2)    (consC (desugar b1) (desugar b2))]
    [carS    (c)        (carC (desugar c))]
    [cdrS    (c)        (cdrC (desugar c))]
    [equal?S (l r)      (equal?C (desugar l) (desugar r))]
    [letS    (s v b) (appC (lamC s (desugar b)) (desugar v))]
    [let*S   (s1 v1 s2 v2 b) (appC (lamC s1 (appC (lamC s2 (desugar b)) (desugar v2))) (desugar v1))]
    ))


; We need storage and location
(define-type-alias Location number)

; We need a new value for the box
(define-type Value
  [numV  (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [boxV  (l : Location)] ; Points to the location
  [consV (car : Location) (cdr : Location)]
  [suspV (body : ExprC) (env : Env)]
  )


; Bindings associate symbol with location
(define-type Binding
        [bind (name : symbol) (val : Location)])

; Env remains the same, we only change the Binding
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)


; Storage's operations are similar to Env's
;   bind <-> cell
;   mt-env <-> mt-store
;   extend-env <-> override-store
(define-type Storage
      [cell (location : Location) (val : Value)])
(define-type-alias Store (listof Storage))

(define mt-store empty)
(define override-store cons)


; lookup changes its return type
(define (lookup [for : symbol] [env : Env]) : Location
       (cond
            [(empty? env) (error 'lookup (string-append (symbol->string for) " não foi encontrado"))] ; livre (não definida)
            [else (cond
                  [(symbol=? for (bind-name (first env)))   ; achou!
                                 (bind-val (first env))]
                  [else (lookup for (rest env))])]))        ; vê no resto

; fetch is equivalent to a lookup for the store
(define (fetch [l : Location] [sto : Store]) : Value
       (cond
            [(empty? sto) (error 'fetch "posição não encontrada")]
            [else (cond
                  [(= l   (cell-location (first sto)))   ; achou!
                                 (cell-val (first sto))]
                  [else (fetch l (rest sto))])]))        ; vê no resto


; Returns the next location available
(define new-loc
   (let ( [ n (box 0)])
        (lambda () 
           (begin
              (set-box! n (+ 1 (unbox n)))
              (unbox n)))))


; Auxiliar operators
(define (num+ [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (+ (numV-n l) (numV-n r)))]
        [else
             (error 'num+ "Um dos argumentos não é número")]))

(define (num* [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (* (numV-n l) (numV-n r)))]
        [else
             (error 'num* "Um dos argumentos não é número")]))

(define (num-equal? [l : Value] [r : Value]) : Value
    (cond
        [(equal? (numV-n l) (numV-n r))
             (numV 1)]
        [else
             (numV 0)]))


; New return type for our interpreter, Env and Store
(define-type Result
      [v*s (v : Value) (s : Store)])

; interp receives and returns Store
(define (interp [a : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC a
    [numC (n) (v*s (numV n) sto)]
    [idC (n)  (v*s (fetch (lookup n env) sto) sto)]; cascading search, first in env then in sto
    [lamC (a b) (v*s (closV a b env) sto)]
    
    [seqC (b1 b2) (type-case Result (interp b1 env sto)
                    [v*s (v-b1 s-b1) ; result and store retorned by b1
                          (interp b2 env s-b1)])]
                          
    ; application of function
    [appC (f a)
      (type-case Result (interp f env sto) ; find the function
         [v*s (v-f s-f)
              (type-case Result (interp a env s-f) ; argument with sto changed by the function
                 [v*s (v-a s-a)
                      (let ([where (new-loc)]) ; allocs position for the value of the argument
                           (interp (closV-body v-f) ; body
                                   (extend-env (bind (closV-arg v-f) where) ; with new argument
                                       (closV-env v-f))
                                   (override-store (cell where v-a) s-a))) ; with new value
                  ])])]
                  
    [plusC (l r) 
           (type-case Result (interp l env sto)
               [v*s (v-l s-l)
                    (type-case Result (interp r env s-l)
                      [v*s (v-r s-r)
                           (v*s (num+ v-l v-r) s-r)])])]
                           
    [multC (l r) 
           (type-case Result (interp l env sto)
               [v*s (v-l s-l)
                    (type-case Result (interp r env s-l)
                      [v*s (v-r s-r)
                           (v*s (num* v-l v-r) s-r)])])]
                           
    ; ifC serializes
    [ifC (c s n) (type-case Result (interp c env sto)
                   [v*s (v-c- s-c)
                        (if (zero? (numV-n (v*s-v (interp c env sto))))
                            (interp n env s-c)
                            (interp s env s-c))])]
    
    ; Working with lists
;    [consC (b1 b2) (type-case Result (interp b1 env sto)
;                     [v*s (v-b1 s-b1)
;                          (type-case Result (interp b2 env s-b1)
;                            [v*s (v-b2 s-b2)
;                                 (let ((where-b1 (new-loc)) (where-b2 (new-loc)))
;                                   (v*s (consV where-b1 where-b2)
;                                        (override-store (cell where-b2 v-b2)
;                                                        (override-store (cell where-b1 v-b1)
;                                                                        s-b2))))])])]

    [carC (c) (type-case Result (interp c env sto)
                [v*s (v-c s-c)
                     (begin
                       ;pegar a location do car
                       ;verificar se eh uma suspensao
                       ;se for, interpretar, se nao, usar o valor
                       (let* ((car-loc (consV-car v-c))
                              (car-value (fetch car-loc s-c)))
                         (begin (display car-loc)
                                (display "\n")
                                (display car-value)
                                (display "\n")
                                (type-case Value car-value
                                  [suspV  (exp env) (interp exp env sto)]
                                  [else (v*s (numV 101) sto)]

                                ))
                       ))])]
         
    [cdrC (c) (type-case Result (interp c env sto)
                [v*s (v-c s-c)
                     (begin
                       ;pegar a location do car
                       ;verificar se eh uma suspensao
                       ;se for, interpretar, se nao, usar o valor
                       (let* ((cdr-loc (consV-cdr v-c))
                              (cdr-value (fetch cdr-loc s-c)))
                         (begin (display cdr-loc)
                                (display "\n")
                                (display cdr-value)
                                (display "\n")
                                (type-case Value cdr-value
                                  [suspV  (exp env) (interp exp env sto)]
                                  [else (v*s (numV 101) sto)]

                                ))
                       ))])]
         

    
    [consC (b1 b2)
           (let ((where-b1 (new-loc))
                 (where-b2 (new-loc))
                 (v-b2 (suspV b2 env))
                 (v-b1 (suspV b1 env)))
             (v*s (consV where-b1 where-b2)
                  (override-store (cell where-b2 v-b2)
                                  (override-store (cell where-b1 v-b1)
                                                  sto))))]

    [equal?C (l r) 
           (type-case Result (interp l env sto)
               [v*s (v-l s-l)
                    (type-case Result (interp r env s-l)
                      [v*s (v-r s-r)
                           (v*s (num-equal? v-l v-r) s-r)])])]

    ))


; Parser with funny instructions for boxes
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (idS (s-exp->symbol s))] ; pode ser um símbolo livre nas definições de função
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(~) (uminusS (parse (second sl)))]
         [(lambda) (lamS (s-exp->symbol (second sl)) (parse (third sl)))] ; definição
         [(call) (appS (parse (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(seq) (seqS (parse (second sl)) (parse (third sl)))]
         [(cons) (consS (parse (second sl)) (parse (third sl)))]
         [(car) (carS (parse (second sl)))]
         [(cdr) (cdrS (parse (second sl)))]
         [(equal?) (equal?S (parse (second sl)) (parse (third sl)))]
         [(let) (letS (s-exp->symbol (first (s-exp->list (first (s-exp->list (second sl)))))) (parse (second (s-exp->list (first (s-exp->list (second sl)))))) (parse (third sl)))]
         [(let*) (let*S (s-exp->symbol (first (s-exp->list (first (s-exp->list (second sl)))))) (parse (second (s-exp->list (first (s-exp->list (second sl)))))) (s-exp->symbol (first (s-exp->list  (second (s-exp->list (second sl)))))) (parse (second (s-exp->list (second (s-exp->list (second sl)))))) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))


; Facilitator
(define (interpS [s : s-expression]) (interp (desugar (parse s)) mt-env mt-store))


; Examples
;(interpS '(+ 10 (call (lambda x (car x)) (cons 15 16))))

;;(interpS '(call (lambda x (seq (:= x (+ x 5))x)) 8))

;;(interpS '(seq (!# (-# 2) 32) (># (-# 2) (+ (># (-# 2)) 10))))

;;(interpS '(call (lambda f (call f (-# 32))) (lambda x (seq (!# x (+ (># x) 10)) (># x)))))


; Tests
;(test (v*s-v (interp (carC (consC (numC 10) (numC 20)))
;              mt-env mt-store))
;      (numV 10))

; MEUS TESTES BÁSICOS
;(test (v*s-v (interpS '(+ 3 1))) (numV 4))


; TESTE EQUAL?
;(display "######## TESTE EQUAL? ########\n")
;(test (v*s-v (interpS '(equal? (+ 1 3) (+ 1 3)))) (numV 1))
;(test (v*s-v (interpS '(equal? (+ 1 3) (+ 1 2)))) (numV 0))
;(test (v*s-v (interpS '(equal? (+ 1 3) (+ 2 2)))) (numV 1))
;(test (v*s-v (interpS '(equal? (+ 10 (call (lambda x (car x)) (cons 15 16)))  (+ 10 (call (lambda x (car x)) (cons 15 16)))) )) (numV 1))

;TESTE LET
;(display "######## TESTE LET ########\n")
;(test (v*s-v (interpS '(let [(x 3)] x))) (numV 3))

;TESTE LET*
;(display "######## TESTE LET* ########\n")
;(test (v*s-v (interpS '(let* [(a 1) (b 1)] (+ a b)))) (numV 2))


; primeiro fazer o coms
; depois o car
; depois o exemplo de aplicar um lambda

; temos que mexer no idc? e no lamC?
; no lamC fazemos suspensao de arg
; entender msg paca e email nayereh e pergunta chagas


;I changed "cdrC", "carC" and "consC" and now results of "(interpS '(cons 4 5))"  and "(interpS '(car (cons (cons 4 5) 6)))" using laze evaluation are in

;order   "(v*s (consV 18 19) (list (cell 19 (suspV (numC 5) '())) (cell 18 (suspV (numC 4) '()))))"         and       "(v*s (consV 16 17) (list (cell 17 (suspV 

;(numC 5) '())) (cell 16 (suspV (numC 4) '())) (cell 15 (suspV (numC 6) '())) (cell 14 (suspV (consC (numC 4) (numC 5)) '()))))" !



;(interpS '(cons 4 5))
;(v*s (consV 21 22) (list (cell 22 (numV 5)) (cell 21 (numV 4))))
;(v*s (consV 21 22) (list (cell 22 (suspV (numC 5) '())) (cell 21 (suspV (numC 4) '()))))
;(interpS '(car (cons (cons 4 5) 6)))
;(v*s (consV 16 17) (list (cell 17 (suspV (numC 5) '())) (cell 16 (suspV (numC 4) '())) (cell 15 (suspV (numC 6) '())) (cell 14 (suspV (consC (numC 4) (numC 5)) '()))))"

;(interpS '(car (cons (+ 1 1) 5)))