#lang Scheme
;precisamos definir combine de novo
(define combine (lambda (soma f zero)
         (lambda (lista)
              (if (null? lista) zero
                   (soma  (f (car lista))
			 	 ((combine soma f zero) (cdr lista)))))))
(define id (lambda (x) x))

(define find (lambda (pred lista)
               (if (null? lista) #f
                   (if  (pred (car lista)) #t 
                        (find pred (cdr lista))))))
(define conj-vazio (lambda () '()))
(define adiciona-elem (lambda (elem conj)
                        (if (member? elem conj) conj (cons elem conj))))
(define member? (lambda (elem conj)
                  (find ((curry =) elem) conj)))
(define uniao (lambda (conj1 conj2)
                ((combine  adiciona-elem id conj2) conj1)))
(define conj1 '(1 2 3 4 5))
(member? 1 conj1)
(define conj2 (adiciona-elem 57 conj1))
conj2
(member? 7 conj2)
(member? 57 conj2)
(member? 5 conj2)
(define conj3  '(3 5  6 7 8 9))
(uniao conj1 conj3)
;resultado ==> (1 2 4 3 5 6 7 8 9)