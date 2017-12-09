#lang plai-typed


; Basic expressions
(define-type ExprC
  [numC  (n : number)]
  [idC   (s : symbol)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [appC  (fun : ExprC) (arg : ExprC)]
  [ifC   (c : ExprC) (y : ExprC) (n : ExprC)]
  [seqC  (e1 : ExprC) (e2 : ExprC)]
  [setC  (var : symbol) (arg : ExprC)]
  [letC  (name : symbol) (arg : ExprC) (body : ExprC)]
  [classC (parent : symbol) (ins-var : symbol) (m1 : ExprC) (m2 : ExprC)]
  [methodC (name : symbol) (arg : symbol) (body : ExprC)]
  [newC (class : symbol) (arg : number)]
  [sendC (obj : ExprC) (method-name : symbol) (param : ExprC)]
  )


; Sugared expressions
(define-type ExprS
  [numS    (n : number)]
  [idS     (s : symbol)]
  [appS    (fun : ExprS) (arg : ExprS)]
  [plusS   (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS   (l : ExprS) (r : ExprS)]
  [ifS     (c : ExprS) (y : ExprS) (n : ExprS)]
  [seqS    (e1 : ExprS) (e2 : ExprS)]
  [setS    (var : symbol) (arg : ExprS)]
  [letS    (name : symbol) (arg : ExprS) (body : ExprS)]
  [classS  (parent : symbol) (ins-var : symbol) (m1 : ExprS) (m2 : ExprS)]
  [methodS (name : symbol) (arg : symbol) (body : ExprS)]
  [newS    (class : symbol) (arg : number)]
  [sendS   (obj : ExprS) (method-name : symbol) (param : ExprS)]
  )


; Removing the sugar
(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
    [numS    (n)        (numC n)]
    [idS     (s)        (idC s)]
    [appS    (fun arg)  (appC (desugar fun) (desugar arg))]
    [plusS   (l r)      (plusC (desugar l) (desugar r))]
    [multS   (l r)      (multC (desugar l) (desugar r))]
    [bminusS (l r)      (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)        (multC (numC -1) (desugar e))]
    [ifS     (c s n)    (ifC (desugar c) (desugar s) (desugar n))]
    [seqS    (e1 e2)    (seqC (desugar e1) (desugar e2))]
    [setS    (var expr) (setC  var (desugar expr))]
    [letS    (n a b)    (letC n (desugar a) (desugar b))]
    [classS  (parent ins-var m1 m2) (classC parent ins-var (desugar m1) (desugar m2))]
    [methodS (name arg body) (methodC name arg (desugar body))]
    [newS    (class arg) (newC class arg)]
    [sendS   (obj method-name param) (sendC (desugar obj) method-name (desugar param))]
    ))


; We need a new value for the box
(define-type Value
  [numV  (n : number)]
  [methodV (name : symbol) (arg : symbol) (body : ExprC)]
  [classV (parent : symbol) (ins-var : symbol) (m1 : Value) (m2 : Value)]
  [objectV (class : symbol) (parent : Value) (obj-env : (listof Binding))]
  )


; Bindings associate symbol with location
(define-type Binding
        [bind (name : symbol) (val : (boxof Value))])

; Env remains the same, we only change the Binding
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

; Add Object in global environment
(define Object (classV 'DummyClass 'dummy_arg (methodV 'dummy_method1 'dummy_arg_1 (numC 0)) (methodV 'dummy_method2 'dummy_arg_2 (numC 0))))
(define env (extend-env
               (bind 'Object (box Object))
               mt-env))

; Find the name of a variable
(define (lookup [for : symbol] [env : Env]) : (boxof Value)
  (begin
    ;(display "symbol: ")
    ;(display for)
    ;(display "\n")
       (cond
            [(empty? env) (error 'lookup (string-append (symbol->string for) " was not found"))] ; variable is undefined
            [else (cond
                  [(symbol=? for (bind-name (first env)))   ; found it!
                                 (bind-val (first env))]
                  [else (lookup for (rest env))])])))        ; check in the rest


; Auxiliary operators
(define (num+ [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (+ (numV-n l) (numV-n r)))]
        [else
             (error 'num+ "One of the arguments is not a number")]))

(define (num* [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (* (numV-n l) (numV-n r)))]
        [else
             (error 'num* "One of the arguments is not a number")]))

; Create object and set instance vars in class
(define (create-object-set-ins-vars class arg env) ; Wallet/0/env
  
  (if (equal? class 'Object)
      (objectV class (objectV 'DummyClass (numV 1) env) env)

      (begin
             (display "variavel de instancia: ") 
             (display  (classV-ins-var  (unbox (lookup class env))))
             (display "\n")
             (let* [(classValue (unbox (lookup class env)))
                    (parentClassId (classV-parent classValue))
                    (extended-env (extend-env (bind (classV-ins-var classValue) (box (numV arg))) env))]
                               (objectV class
                                        (create-object-set-ins-vars parentClassId 0 env)
                                        extended-env)))))

(define (find-method obj method-name env)
  (begin ;(display "Environment: ")
         ;(display env)
         ;(display "\n\n")

         ;(display "Object ")
         ;(display obj)
         ;(display "\n\n")

         ;(display "Parent: ")
         ;(display (objectV-parent obj))
         ;(display "\n\n")

         
         (let* ([class (objectV-class obj)]
                [classValue (unbox (lookup class env))]
                [parentClass (objectV-class (objectV-parent obj))])
           (if (equal? parentClass 'DummyClass)
               (error 'find-method (string-append "Class does not respond to the method " (symbol->string method-name)))

               (if (equal? (methodV-name (classV-m1 classValue)) method-name)
                   (classV-m1 classValue)
                   (if (equal? (methodV-name (classV-m2 classValue)) method-name)
                       (classV-m2 classValue)
                       (find-method (objectV-parent obj) method-name env)))))))
                        
           
           
         
; Interpreter
(define (interp [a : ExprC] [env : Env]) : Value
  (type-case ExprC a
    ; Numbers just evaluta to their equivalent Value
    [numC (n) (numV n)]

    ; IDs are retrieved from the Env and unboxed
    [idC (n) (unbox (lookup n env))]
    
    ; Application of function
    [appC (f a) (numV 99999)]
              
;    [appC (f a)
;          (let ([f-value (interp f env)])
;            (interp (methodV-body f-value)
;                    (extend-env
;                        (bind (methodV-arg f-value) (box (interp a env)))
;                        (methodV-env f-value)
;                    )))]

    ; Sum two numbers using auxiliary function
    [plusC (l r) (num+ (interp l env) (interp r env))]

    ; Multiplies two numbers using auxiliary function
    [multC (l r) (num* (interp l env) (interp r env))]

    ; Conditional operator
    [ifC (c s n) (if (zero? (numV-n (interp c env))) (interp n env) (interp s env))]

    ; Sequence of operations
    [seqC (b1 b2) (begin (interp b1 env) (interp b2 env))] ; No side effect between expressions!

    ; Attribution of variables
    [setC (var val) (let ([b (lookup var env)])
                      (begin (set-box! b (interp val env)) (unbox b)))]

    ; Declaration of variable
    [letC (name arg body)
          (let* ([new-bind (bind name (box (interp arg env)))]
                 [new-env (extend-env new-bind env)])
            (interp body new-env))]

    [classC (parent ins-var m1 m2) (classV parent ins-var (interp m1 env) (interp m2 env))]

    [methodC (name arg body) (methodV name arg body)]

    [newC (class arg) (let* ([result (create-object-set-ins-vars class arg env)])
                        (begin
                          ;(display "Result: ")
                          ;(display result)
                           result))]

    [sendC (obj method-name param) (let* ([objectValue (interp obj env)]
                                          [method (find-method objectValue method-name env)]
                                          [method-body (methodV-body method)]
                                          [method-arg (methodV-arg method)])
                                          (interp method-body
                                                  (extend-env (bind 'self (box objectValue))
                                                              (extend-env
                                                                 (bind method-arg (box (interp param env)))
                                                                 (objectV-obj-env objectValue)))))]
    ))


; Parser
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (idS (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(~) (uminusS (parse (second sl)))]
         ;[(lambda) (lamS (s-exp->symbol (second sl)) (parse (third sl)))] ; definição
         ;[(call) (appS (parse (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(seq) (seqS (parse (second sl)) (parse (third sl)))]
         [(:=) (setS (s-exp->symbol (second sl)) (parse (third sl)))]
         [(let) (letS (s-exp->symbol (first (s-exp->list (first (s-exp->list (second sl))))))
                      (parse (second (s-exp->list (first (s-exp->list (second sl))))))
                      (parse (third sl)))]
         [(method) (methodS (s-exp->symbol (second sl)) (s-exp->symbol (third sl))  (parse (fourth sl)))]
         [(class) (classS (s-exp->symbol (second sl)) (s-exp->symbol (third sl)) (parse (fourth sl)) (parse (fourth (rest sl))))]
         [(new) (newS (s-exp->symbol (second sl)) (s-exp->number (third sl)))]
         [(send) (sendS (parse (second sl)) (s-exp->symbol (third sl)) (parse (fourth sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))


; Facilitator
(define (interpS [s : s-expression]) (interp (desugar (parse s)) env))


; Examples
;(interpS '(let ([x 10]) x))


;(interpS '(new ObjectA 0)) ; It works because class object already exists in global env

; Test #0: Method call when instantiating Object
(test/exn
  (interpS
    '(let ([obj (new Object 0)])
       (send obj blah 42))) ; <-- Method does not exist!
  "Class does not respond to the method blah")



; Test #1: User-defiend class inheriting from Object, with methods that change
;          the attribute of the object (shared between them).
(test
  (interpS
    '(let ([Wallet
             (class Object money
                    (method credit amount (:= money (+ money amount)))
                    (method debit amount (:= money (- money amount))) )])
       (let ([wallet (new Wallet 0)])
         (seq (send wallet credit 10)
              (send wallet debit 3)))))
  (numV 7))




; Test #2: User-defined class inheriting from Object, with method that delegates
;          to another via self.
(test
  (interpS
    '(let ([Wallet
             (class Object money
                    (method credit amount (:= money (+ money amount)))
                    (method debit amount (send self credit (~ amount))) )])
       (let ([wallet (new Wallet 0)])
         (seq (send wallet credit 10)
              (send wallet debit 3)))))
  (numV 7))

; Test #3: User-defined class inheriting from Object, calling a method that does
;          not exist.
(test/exn
  (interpS
    '(let ([Wallet
             (class Object money
                    (method credit amount (:= money (+ money amount)))
                    (method debit amount (send self credit (~ amount))) )])
       (let ([wallet (new Wallet 0)])
         (seq (send wallet credit 10)
              (send wallet deduction 3))))) ; <-- Method does not exist!
  "Class does not respond to the method deduction")

; Test #4: User-define class inheriting from another user-defined class,
;          with method from child overriding the parent's implementation,
;          method from parent delegating to overriden method, and method
;          from child accessing attribute of the parent.
(test
  (interpS '(let ([Wallet
                    (class Object money
                           (method credit amount (:= money (+ money amount)))
                           (method debit amount (send self credit (~ amount))) )])
              (let ([WalletWithTaxes
                      (class Wallet tax
                             (method credit amount (:= money (- (+ money amount) tax)))
                             (method total dummy money) )])
                (let ([wallet (new WalletWithTaxes 1)])
                  (seq (send wallet credit 10)
                       (seq (send wallet debit 3)
                            (send wallet total 0))
                       )))))
  (numV 5))

;; Test #5: User-define class inheriting from another user-defined class,
;;          calling a method that does not exist.
;(test/exn
;  (interpS '(let ([Wallet
;                    (class Object money
;                           (method credit amount (:= money (+ money amount)))
;                           (method debit amount (send self credit (~ amount))) )])
;              (let ([WalletWithTaxes
;                      (class Wallet tax
;                             (method credit amount (:= money (- (+ money amount) tax)))
;                             (method total dummy money) )])
;                (let ([wallet (new WalletWithTaxes 1)])
;                  (seq (send wallet credit 10)
;                       (seq (send wallet debit 3)
;                            (send wallet amount 0)) ; <-- Method does not exist!
;                       )))))
;  "Class does not respond to the method amount")