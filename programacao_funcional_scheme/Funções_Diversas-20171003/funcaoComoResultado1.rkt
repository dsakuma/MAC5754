#lang Scheme
(define soma-fixa (lambda (incremento)
       (lambda (x) (+ x incremento))))
(define soma-1 (soma-fixa 1))






; agora um exemplo mais geral: derivada de uma funcao qualquer
(define deriva (lambda (f dx)
   (lambda (x) (/ (- (f (+ x dx) (f x)))
                      dx))))
