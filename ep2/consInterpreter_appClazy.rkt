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
      [cell (location : Location) (val : (boxof Value))])
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
(define (fetch [l : Location] [sto : Store]) : Result
       (begin
;         (display "inicio fetch \n")
;         (display "l ->")
;         (display l)
;         (display "\n")
;         (display "cell-location -> \n")
;         (display (cell-location (first sto)))
;         (display "\n")
       (cond
            [(empty? sto) (error 'fetch "posição não encontrada")]
            [else (cond
                  [(= l (cell-location (first sto)))   ; achou!
                                 (begin
;                                   (display "first sto -> ")
;                                   (display (first sto))
;                                   (display "\n")
;                                   (display "sto ->")
;                                   (display sto)
;                                   (display "\n")
;                                   (display "cell-val first sto ->")
;                                   (display (cell-val (first sto)))
;                                   (display "\n")
                                    (let* ((unboxed-val (unbox (cell-val (first sto)))) 
                                           (result (type-case Value unboxed-val
                                                    [suspV (body env) (interp body env sto)]
                                                    [else (v*s unboxed-val sto)])))
                                      (begin
                                        (set-box! (cell-val (first sto)) (v*s-v result))
                                        result)))]
                  [else (fetch l (rest sto))])])))        ; vê no resto


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
    [idC (n)  (fetch (lookup n env) sto)]; cascading search, first in env then in sto
    [lamC (a b) (v*s (closV a b env) sto)]
    
    [seqC (b1 b2) (type-case Result (interp b1 env sto)
                    [v*s (v-b1 s-b1) ; result and store retorned by b1
                          (interp b2 env s-b1)])]
                          
    ; application of function
;    [appC (f a)
;      (type-case Result (interp f env sto) ; find the function
;         [v*s (v-f s-f)
;              (type-case Result (interp a env s-f) ; argument with sto changed by the function
;                 [v*s (v-a s-a)
;                      (let ([where (new-loc)]) ; allocs position for the value of the argument
;                           (interp (closV-body v-f) ; body
;                                   (extend-env (bind (closV-arg v-f) where) ; with new argument
;                                       (closV-env v-f))
;                                   (override-store (cell where (box v-a)) s-a))) ; with new value
;                  ])])]

    [appC (f a)
      (type-case Result (interp f env sto) ; find the function
         [v*s (v-f s-f)
              ; nao interpretar o exp a
              ; criar um thunk, guardar em memoria
              (let ((where-a (new-loc))
                    (v-a (suspV a env)))
                (interp (closV-body v-f) ; body
                                   (extend-env (bind (closV-arg v-f) where-a) ; with new argument
                                       (closV-env v-f))
                                   (override-store (cell where-a (box v-a)) s-f)))])]

    
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


   
    [carC (c) (type-case Result (interp c env sto)
                [v*s (v-c s-c)
                     (begin
                       (fetch (consV-car v-c) s-c))])]
    
    
    [cdrC (c) (type-case Result (interp c env sto)
                [v*s (v-c s-c)
                     (fetch (consV-cdr v-c) s-c)])]


    [consC (b1 b2)
           (let ((where-b1 (new-loc))
                 (where-b2 (new-loc))
                 (v-b2 (suspV b2 env))
                 (v-b1 (suspV b1 env)))
             (v*s (consV where-b1 where-b2)
                  (begin
                  (override-store (cell where-b2 (box v-b2))
                                  (override-store (cell where-b1 (box v-b1))
                                                  sto))
                  )))]

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


; Tests
;(test (v*s-v (interp (carC (consC (numC 10) (numC 20)))
;              mt-env mt-store))
;      (numV 10))




(test (v*s-v (interpS '(cons (* 2 2) (* 3 1))))
      (consV 1 2))

(test (v*s-v (interpS '(car (cons (* 2 2) (* 3 1)))))
      (numV 4))

(test (v*s-v (interpS '(cdr (cons (* 2 2) (* 3 1)))))
      (numV 3))



(test (v*s-v (interpS '(let* [(a (cons (+ 1 2) (* 4 1))) (b (+ (car a) (car a)))] (* b 2))))
      (numV 12))

(test (v*s-v (interpS '(let* [(a (cons (+ 1 2) (* 4 1))) (b (+ (car a) 9))] (* b 2))))
      (numV 24))

(test (v*s-v (interpS '(let* [(a (cons (+ 1 2) (* 4 1))) (b (+ (car a) (car a)))] (* b 2))))
      (numV 12))

(test (v*s-v (interpS '(+ (car (cons (+ 1 2) (* 4 1))) 10)))
      (numV 13))

(test (v*s-v (interpS '(+ (let* [(fact (lambda n n)) (b (call fact 11))] b) 0)))
      (numV 11))

(test (v*s-v (interpS '(+ (let* [(fact (lambda n n)) (b (call fact 11))] (+ b (call fact 1))) 0)))
      (numV 12))

(test (v*s-v (interpS '(+ (let* [(fact (lambda n n)) (b (call fact 11))] (+ (call fact 1) b)) 0)))
      (numV 12))

(test (v*s-v (interpS '(* (let* [(fact (lambda n n)) (b (call fact 11))] b) 5)))
      (numV 55))

(test (v*s-v (interpS '(* (let* [(fact (lambda n n)) (b (call fact 11))] (* b (call fact 1))) 1)))
      (numV 11))

(test (v*s-v (interpS '(* (let* [(fact (lambda n n)) (b (call fact 11))] (* (call fact 1) b)) 1)))
      (numV 11))

(test (v*s-v (interpS '(let* [(fact (lambda n n)) (b (call fact 11))] b)))
      (numV 11))

(test (v*s-v (interpS '(let [(fact (lambda n (+ n 1)))] (call fact 33))))
      (numV 34))

