#lang scheme
(define (filter x l)
  (if (null? l)
      '()
      (if (equal? x (car l))
          (cons (car l) (filter x (cdr l)))
          (filter x (cdr l)))))

(define (length l)
  (if (null? l)
      0
      (+ 1 (length (cdr l)))))
  

(define (count x l)
  (length (filter x l)))

(define atom?
  (lambda (x)
    (if (null? x)
        #t
        (not (list? x)))))

(define (flatten l)
  (if (null? l)
      '()
      (if (not (pair? l))
          (list l)
          (append (flatten (car l)) (flatten (cdr l))))))

(define (countall x l)
  (count x (flatten l)))
