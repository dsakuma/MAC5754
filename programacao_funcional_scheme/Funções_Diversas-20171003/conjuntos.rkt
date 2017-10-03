#lang Scheme
(define conj-vazio '()) ; obs: ou (define conj-vazio () ´())
(define (membro? elem conj)
  (if (null? conj) #f
      (if (equal? elem (car conj)) #t 
          (membro? elem (cdr conj)))))
(define (adiciona elem conj)
  (if (membro? elem conj) conj (cons elem conj)))
(define (uniao conj1 conj2)
  (if (null? conj1) conj2
      (uniao (cdr conj1) (adiciona (car conj1) conj2))))
