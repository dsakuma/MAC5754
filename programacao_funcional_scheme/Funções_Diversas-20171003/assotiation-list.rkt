#lang Scheme

(define (assoc chave lista)
  (if (null? lista) '()
      (if (= chave (caar lista)) 
          (cadar lista) ; obs: e se trocarmos “=” por “equal?” ?
          (assoc chave (cdr lista)))))
(define (mkassoc chave valor lista)
  (if (null? lista)
      (list (list chave valor))
      (if (equal? chave (caar lista))
          (cons (list chave valor ) 
                (cdr lista))
          (cons (car lista)
                (mkassoc chave valor (cdr lista))))))
