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
      (if (not (list? l))
          (list l)
          (append (flatten (car l)) (flatten (cdr l))))))

(define (countall x l)
  (count x (flatten l)))


(define reverse
  (lambda (l)
    (if (null? l)
        '()
        (append (reverse (cdr l)) (list (car l))))))

(define twist
  (lambda (l)
    (display l) (newline)
    (if (null? l)
        '()
        (if (list? (car l))
            (append (twist (cdr l)) (twist (car l)))
            (append (twist (cdr l)) (car l))))))



(display 'countall) (newline)
(countall 'a '(a))
(countall 'a '(a b a))

(display 'reverse) (newline)
(reverse '())
(reverse '(a))
(reverse '(a b (c d)))

(display 'twist) (newline)
(twist '())
(twist '(a))
(twist '((a)))
