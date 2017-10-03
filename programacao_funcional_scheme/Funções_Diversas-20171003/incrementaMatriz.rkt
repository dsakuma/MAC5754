#lang Scheme
(define incrementa-matriz  (lambda (m)
      (mapcar (lambda(linha) (mapcar soma1 linha))
                    m)))

(define incrementa-matriz2 (lambda (m)
       (mapcar (mapc soma1) m)))
(define incrementa-matriz3  (mapc (mapc soma1)))


(define matriz-diagonal '((1 0 0 0) (0 1 0 0) (0 0 1 0) (0 0 0 1)))

(define matriz-qualquer '((1 2 3 4)(1 2 3 4) (0 0 0 0) (1 1 1 10)))



                          


                     
