#lang Scheme
(define mapcar (lambda (func l)
                 (if (null? l) '()
                     (cons (func (car l))
                           (mapcar func (cdr l))))))
(define curry (lambda (f)
      (lambda (x)  
               (lambda (y) (f x y)))))
(define mapc (curry mapcar))
(define soma1 ((curry +) 1))
(define soma1* (mapc soma1))
(define soma1** (mapc soma1*))

;agora vamos brincar, primeiro defino uma lista e uma lista de listas
(define lista '(3 4 5 6))
(define listalista '((3 4 5) (5 6 7)))

;agora a diversao vamos aplicar
(begin (display "aplicar soma1* em ")(displayln lista))
(soma1* lista)
(begin (display "aplicar soma1** em ")(displayln listalista))
(soma1** listalista)


; usando mapcar e mac c em matrizes
(define incrementa-matriz  (lambda (m)
      (mapcar (lambda(linha) (mapcar soma1 linha))
                    m)))

(define incrementa-matriz2 (lambda (m)
       (mapcar (mapc soma1) m)))
(define incrementa-matriz3  (mapc (mapc soma1)))


(define matriz-diagonal '((1 0 0 0) (0 1 0 0) (0 0 1 0) (0 0 0 1)))

(define matriz-qualquer '((1 2 3 4)(1 2 3 4) (0 0 0 0) (1 1 1 10)))
