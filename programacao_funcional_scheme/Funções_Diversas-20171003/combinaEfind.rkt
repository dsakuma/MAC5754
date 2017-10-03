#lang Scheme
(define combine (lambda (soma f zero) 
         (lambda (lista) 
              (if (null? lista) zero
                  (soma  (f (car lista))
                         ((combine soma f zero) (cdr lista)))))))


; usando combine  
(define id (lambda (x) x))
(define produtoria (combine * id 1))
(define mapcar (lambda (f l) ((combine cons f '())l)))

;agora o find
(define find (lambda (pred lista)
               (if (null? lista) #f
                   (if (pred (car lista)) #t
                       (find pred (cdr lista))))))






(define listanums '(1 1 1 1))

