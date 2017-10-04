#lang scheme

(define (mapcar f l)
  (if (null? l)
      '()
      (cons (f (car l)) (mapcar f (cdr l)))))

(define (cdr* l)
  (mapcar cdr l))

(cdr* '((a b c) (d e) (f)))





(define (combine f zero l)
  (if (null? l)
      zero
      (f (car l) (combine f zero (cdr l)))))

(define (max a b)
  (if (> a b) a b))

(define (max* l)
  (combine max 0 l))

(max* '(1 2 3 4 5 20 3))




(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

(append '(1 2) '(3 4))

(define (addtoend x l)
  (append l (cons x '())))

(addtoend 1 '(2 3))


(define (reverse l)
  (if (null? l)
      '()
      (addtoend (car l)
                (reverse (cdr l)))))

(reverse '(a b c))


(define (insertion-sort l )
  (if (null? l) l
      (insert (car l)
              (insertion-sort (cdr l)))))

(define (insert x l)
  (if (null? l) (list x)
      (if (< x (car l)) (cons x l)
          (cons (car l) (insert x (cdr l))))))


(insertion-sort '(4 3 2 1))



(define mapc (curry mapcar))

(define mkpairfn
  (lambda (x)
    (lambda (l)
      (mapcar (lambda (l) (cons x l)) l))))

((mkpairfn 'a) '(() (b c) (d) ((e f))))