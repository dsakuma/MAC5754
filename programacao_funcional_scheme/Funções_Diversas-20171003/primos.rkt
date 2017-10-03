#lang Scheme

(define (divide? m n) (= (modulo n m) 0))
(define (intervalo m n) 
  (if (> m n)
      '()
      (cons m (intervalo (+ 1 m) n))))
(define (remove-multiplos num lista)
  (if (null? lista) lista
      (if (divide? num (car lista)) 
          (remove-multiplos num (cdr lista))
          (cons (car lista) (remove-multiplos num (cdr lista))))))
(define (filtro lista)
  (if (null? lista) lista
      (cons (car lista) (remove-multiplos (car lista) (filtro (cdr lista))))))
(define (primos<= n) (filtro (intervalo 2 n)))
