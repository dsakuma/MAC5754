#lang Scheme

(define (insertion-sort  l )
  (if  (null? l) l 
       (insert (car l) 
               (insertion-sort (cdr l)))))
(define (insert x l)
  (if (null? l) (list x)
      (if  (< x (car l)) (cons x l)
           (cons (car l) (insert x (cdr l))))))
(insertion-sort ' (4 3 1 2))
