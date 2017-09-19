#lang scheme

(define (atom? x)
  (if (null? x)
      #t
      (not (list? x))))

(define count
  (lambda (x l)
    (if (null? l)
        0
       (if (equal? x (car l))
          (+ 1 (count x (cdr l)))
          (count x (cdr l))))))
               

(define countall
  (lambda (x l)
    (if (null? l)
        0
        (if (atom? l)
            (if (equal? x l)
                1
                0)
            (+ (countall x (car l)) (countall x (cdr l)))))))

(define reverse
  (lambda (l)
    (if (null? l)
        '()
        (append (reverse (cdr l)) (list (car l))))))


(define twist
  (lambda (l)
    (if (null? l)
        '()
        (if (atom? l )
            l
            (append (twist (cdr l)) (list (twist (car l))))))))


(define (flatten l)
  (if (null? l)
      '()
      (if (atom? l)
            (list l)
            (append (flatten (car l)) (flatten (cdr l))))))

(define (length l)
  (if (null? l)
      0
      (+ 1 (length (cdr l)))))

(define (contains? x l)
  (if (null? l)
      #f
      (if (equal? x (car l))
          #t
          (contains? x (cdr l)))))

(define (sublist-aux l1 l2)
  (if (null? l1)
      0
      (if (contains? (car l1) l2)
          (+ 1 (sublist-aux (cdr l1) l2))
          (sublist-aux (cdr l1) l2))))

(define (sublist l1 l2)
  (= (length l1) (sublist-aux l1 l2)))

;(define (contig-sublist-aux l1 l2 target count)
;  (if (null? l1)
;      0
;      (if (= target count)
;          count
;          (if (contains? (car l1) l2)
;              (contig-sublist-aux (cdr l1) l2 target (+ count 1))
;              (contig-sublist-aux (cdr l1) l2 target 0)))))

;(define (contig-sublist l1 l2)
;  (= (length l1) (contig-sublist-aux l1 l2 (length l1) 0)))

(define remove2
  (lambda (x s)
    (if (null? s)
        '()
        (if (atom? s)
            (if (not (equal? x s))
                (list s)
                '())
            (append (remove x (car s)) (remove x (cdr s)))))))

(define remove3
  (lambda (x s)
    (if (null? s)
        '()
        (if (atom? s)
            (if (not (equal? x s))
                s
                '())
            (append (list (remove x (car s))) (remove x (cdr s)))))))

(define remove
  (lambda (x s)
    (if (null? s)
        '()
        (if (atom? s)
            s
            (if (equal? x (car s))
                (cons (remove x (car (cdr s))) (remove x (cdr (cdr s))))
                (cons (car s) (remove x (cdr s))))))))
        



(count 'a '(1 b a a (c a) a b))
(countall 'a '(1 b a a (c a (b a)) a b))
(reverse '(1 2 3 4 5 (6 7)))
(twist '(1 2 3 4 5 ))
(flatten '((a b) ((c d) e)))
(sublist '(a b c) '(x a y b z c))
;(contig-sublist '(a b c) '(x a y b z c))
;(contig-sublist '(a y) '(x a y b z c))
(remove 'a '(a b c (a d) e a f))
(remove 'a '(a))