#lang Scheme
;de novo compara-pares

(define compara-pares (lambda (par1 par2)
      (if (< (car par1) (car par2)) #t
          (if (< (car par2) (car par1)) #f
              (< (cadr par1) (cadr par2))))))


(define compara-pares-de-pares
  (lambda (t1 t2) 
    (if (compara-pares (car t1) (car t2)) #t
        (if (compara-pares (car t2) (car t1)) #f
                           (compara-pares (cadr t1) (cadr t2))))))
;função acima é o mesmo que estender o “<“ de compara-pares:
(define ordem-lexicografica-pares
  (lambda (<1 <2)
    (lambda (p1 p2)
      (if (<1 (car p1) (car p2)) #t
          (if (<1 (car p2) (car p1))  #f
              (<2 (cadr p1) (cadr p2)))))))

(define compara-pares2 (ordem-lexicografica-pares < <))
(define compara-pares-de-pares2 
  (ordem-lexicografica-pares compara-pares2 compara-pares2))

;para que dois argumentos: ordens diferentes para clientes diferentes:
(define ordem-estudantes (ordem-lexicografica-pares < >)) 



;vamos usar?
;primeiro pegamos novamente nosso sort2

(define sort2 ( lambda (num1 num2 comparacao)
    (if (comparacao num1 num2) (list num1 num2)
        (list num2 num1))))

(sort2 '(85 1005) '(95 20010) ordem-estudantes)
;resultado ==>((85 1005) (95 200010))
(sort2 '(97 100) '(97 200) ordem-estudantes)
;resultado ==> ((97 200) (95 100))

;nem sempre criar uma funcao de inversao de ordem pode ser trivial
;a nao ser que utilizemos o abaixo

(define inverte-ordem (lambda (<) 
            (lambda (x y) (< y x))))

(define nova-ordem-estudantes (inverte-ordem ordem-estudantes))

(sort2 '(85 1005) '(95 20010) nova-ordem-estudantes)
;resultado ==> ((95 20010) (85 1005))
(sort2 '(97 100) '(97 200) nova-ordem-estudantes)
;resultado ==> ((97 100) (97 200))
