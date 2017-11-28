#lang racket

; Simple example of call/cc

(define f ;do not use k
  (lambda (k)
    (begin
      (+ 5 5))
    ))

(define f2 ;use k
  (lambda (exit)
    (begin
      (exit 5))
    ))


(- (+ (call/cc f) 2) 1) ;return 9
(- (+ (call/cc f2) 2) 1)  ;return 8


;(lambda (x) (- (+ x 2) 1) ;k -> continuation

;----------------

; Question 4 - Exam 1

(define pred
  (lambda (x)
    (if (equal x 999)
        #t
        #f)))

(define combine-continuation
  (lambda (fb fu zero l pred)
    (lambda (l)
      (call/cc (lambda (exit)
                 (letrec ([ccaux lambda(fb fu zero l pred)
                                (if (null? l) zero
                                    (if (pred (car l))
                                        (exit zero)
                                        (fb (fu (car l)) (ccaux fb fu zero (cdr l) pred))))])
                          (ccaux fb fu zero l pred)))))))

(define mycont (combine-continuation fb fu zeero l pred)) ; how to call combine-continuation
(+ (mycont l) 4)

;----------------

; Example of maximum common divisor in slides:

(- (mdc-callcc* l) 1)
(lambda (x) (- x 1))

;--------
(define myset nullset)

(myset 'a) ;#f, '()

(myset 'a) ;#t

(member? elem set); #t if elem is part of set

(define member?
  (lambda (elem set)
    (set elem)))

;----------

(define myset
  (lambda (x)
    (equal? x 1)))

(myset 1);#t
(myset 2);#f


(define (includes-element set elem)
  (lambda (x)
    (if (equal? x elem)
        #t
        (set x))))

(set! myset (includes-element myset 2))
(displayln "-----")
(myset 1);#t
(myset 2);#t

