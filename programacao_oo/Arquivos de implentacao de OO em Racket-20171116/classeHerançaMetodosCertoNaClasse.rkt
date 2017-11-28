#lang racket

#| Correction suggestions (by Fabrício):
 | - Initial value of valorA changed to 1
 | - Included methods valorB and valorB: in ClasseB
 |
 | There are some diferences related to how classeB inherits from classeA and how
 | classeA inherits from Object, but since they don't affect the functionality I am leaving it as is.
 |#



;now the final part of a Smalltalk style implementation
;all classes are sublcasses of some class, or of class Object
;class object implements what happens when an object receives a message
;that is not implemented in the hiearchy.
;in this case, send to the original receive the message 'messageNotUnderstood
;with two arguments: the original message and the parameters.`
;Now subclasses can implement this message and have a special error treatment
;if not Object implments the 'messageNotUnderstood message by returning an error message

;now to send a message, get the class from the object and use the 'send operation of the class
(define Send (lambda (obj msg parlist)
               (let ([class (obj 'Class '() )])
                 ((class 'send) obj msg parlist)))) 
;Object class is the root,does not have instance variables
;here it implements only one message and the treatment of messages not implemented
;in real Smalltalk Object holds many basic functions
(define Object
  (letrec (
           [eumesma (lambda (operacao)
                      (case operacao 
                        ;nao implementa mensagem nenhuma, mas se recebe uma mensagem
                        ;manda para o receptor a mensagem 'messageNotUnderstood com o nome original e
                        ;os parametros. Subclasses podem tratar esta mensagem
                        [(send) (lambda (obj msg parlist)
                                       (case msg
                                         [(messageNotUnderstood)
                                               (let* ([methodName (symbol->string (car parlist))]
                                                      [description "ERROR->messageNotImplemented:"]
                                                      [errorMessage (string-append description methodName)])
                                                      (error errorMessage))]
                                         [else (Send obj 'messageNotUnderstood (list msg parlist))]))]
                        [(new) (letrec ( [Class eumesma]
                                         [eumesmo (lambda (msg parlist) eumesmo)]) ; < this doesn't makes sense, but also does not affect the working of this example...
                                         eumesmo)]
                        ))])
    eumesma))
                       
(define ClasseA 
    ;first the Class environment, which are the methods and the references to the superclass
    (letrec ([superclass Object]
             [_duplicaValor (lambda (self parlist)(self 'valorA: (list (* (self 'valorA '()) 2))))]
             [_m1 (lambda (self parlist) (+ 5 (Send self 'm2 '())))]
             [_m2 (lambda (self parlist) 10)]
             ;to access the instance variable, we need to ask the object itself
             [_valorA (lambda (self parlist) (self 'valorA '()))]
             ;a class is a closure that creates objects and implement message send
             [euMesma  (lambda (operacao)
                          (case operacao
                            [(send)  (lambda (obj msg parlist)
                                       (case msg
                                         [(m1) (_m1 obj parlist)]
                                         [(m2) (_m2 obj parlist)]
                                         [(duplicaValor) (_duplicaValor obj parlist)] ;ClasseA just allows valorA to be modified through this method
                                         [(valorA) (_valorA obj parlist)] 
                                         [else ((superclass 'send) obj msg parlist)]))]
                            [(new) (letrec ( [Class euMesma]
                                             [valorA 1]
                                             [eumesmo (lambda (msg parlist)
                                                        ;the "methods" of the object are only used by the class
                                                        ;they are used to access and set the values of the instance
                                                        ;variables
                                                        ;for every instance variable you should implement
                                                        ;the access and setter functions
                                                        (case msg
                                                          [(Class) Class]
                                                          [(valorA:) (set! valorA (car parlist))]
                                                          [(valorA) valorA]))])
                                     
                                     eumesmo)]
                            ))])
      euMesma))
          



(define ClasseB
  (letrec ([mamae ClasseA]
           [_m2 (lambda (self parlist) 5)]
           [_valorB (lambda (self parlist) (self 'valorB '()))]
           [_valorB: (lambda (self parlist) (self 'valorB: parlist))]
           [eumesma (lambda (operacao)
                      (case operacao
                        [(send)  (lambda (obj msg parlist)
                                       (case msg
                                         [(m2) (_m2 obj parlist)]
                                         [(valorB) (_valorB obj parlist)]
                                         [(valorB:) (_valorB: obj parlist)] ;ClasseB allows valorB to be modified directly
                                         [else ((mamae  'send) obj msg parlist)]))]
                        [(new) (letrec ( [Class eumesma]
                                         [Superobj (mamae 'new)]
                                         [valorB 0]
                                         [eumesmo (lambda (msg parlist)
                                                    (case msg
                                                      [(Class) Class]
                                                      [(valorB) valorB]
                                                      [(valorB:) (set! valorB (car parlist))]
                                                      [else (Superobj msg parlist)]))])
                                 eumesmo)]))])
    eumesma))
;now to create an object call the 'new operation on the class
(define obA (ClasseA 'new))
(define obB (ClasseB 'new))
;now the results will be as expected: m1 for obA will return 15 and for obB will return 10
(Send obA 'm1 '())
(Send obA 'm2 '())
(Send obB 'm1 '())
(Send obB 'm2 '())
;we can also set and update the instance variables in the obB
(Send obB 'valorA '())
;obs the method valorA: does not return any value
;(Send obB 'valorA: '(200)) < valorA cannot be modified in this way!
(Send obB 'duplicaValor '())
(Send obB 'valorA '())
(Send obB 'valorB '())
;obs the method valorB: does not return any value
(Send obB 'valorB: '(500))
(Send obB 'valorB '())

