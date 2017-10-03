#lang Scheme


;primeiro pegamos novamente nosso sort2

(define sort2 ( lambda (num1 num2 comparacao)
    (if (comparacao num1 num2) (list num1 num2)
        (list num2 num1))))



(define n-esimo (lambda (n lista)
                  (if (= n 1) (car lista)
                      (n-esimo (- n 1) (cdr lista) ))))
(define seleciona-colunas (lambda (num-col-1 num-col-2)
       (lambda (l)
             (list (n-esimo num-col-1 l) (n-esimo num-col-2 l)))))
(define compoe-2-1 (lambda (f g) 
       (lambda (x y) (f (g x) (g y)))))
(define compara-colunas (lambda (< num-col1 num-col2)
       (compoe-2-1 < (seleciona-colunas num-col1 num-col2))))



(define compoe-1-1 (lambda (f g) (lambda (x) (f (g x)))))
(define aplica-em-dupla (lambda (f)
         (lambda (l) (f (car l) (cadr l)))))
(define melhora (compoe-1-1 (aplica-em-dupla -) (seleciona-colunas 3 2) ))
(define compara-melhora (compoe-2-1 > melhora))
(sort2 '(Alan 1005 9 10) '(Mitchell 1008 4 9) compara-melhora)
;resultado==> ((Mitchell 1008 4 9) (Alan 1005 9 9))
