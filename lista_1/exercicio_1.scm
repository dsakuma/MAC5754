#lang scheme

;Lista 1.1

(define sigma
  (lambda (m n)
    (if (equal? m n)
        n
        (+ m (sigma (+ m 1) n)))))


(sigma 1 4)

;Lista 1.2

(define exp
  (lambda (m n)
    (if (equal? n 0)
        1
        (* m (exp m (- n 1))))))

(exp 2 4)

(define log
  (lambda (m n)
    (if (> m n)
        1
        (+ 1 (log (* m m) n)))))

(log 10 1000)

(define fib
  (lambda (m)
    (if (equal? m 0 )
        0
        (if (equal? m 1)
            1
            (+ (fib (- m 1)) (fib (- m 2)))))))

(fib 3)