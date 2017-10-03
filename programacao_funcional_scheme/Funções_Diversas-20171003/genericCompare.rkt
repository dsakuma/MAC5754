#lang Scheme
(define sort2 ( lambda (num1 num2 comparacao)
    (if (comparacao num1 num2) (list num1 num2)
        (list num2 num1))))


(define compara-pares (lambda (par1 par2)
      (if (< (car par1) (car par2)) #t
          (if (< (car par2) (car par1)) #f
              (< (cadr par1) (cadr par2))))))
