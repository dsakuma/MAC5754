#lang scheme
(define init-rand (lambda (valor-inicial)
                    (lambda () (begin
                                 (set! valor-inicial
                                       (remainder (+ (* valor-inicial 9) 5) 1024))
                                 valor-inicial))))

(define rand (init-rand 1))



(define (mdc a b)
  (displayln 'mdc)
  (cond [(= b 0) a]
        [else (mdc b (modulo a b))]))

(define mdc*
  (lambda (lista)
    (if (= (car lista) 1) 1
        (if (null? (cdr lista)) (car lista)
            (mdc (car lista) (mdc* (cdr lista)))))))






(define mdc-melhor*
  (lambda (lista )
    (if (= (car lista) 1) 1
        (mdc*-aux (car lista) (cdr lista)))))

(define mdc*-aux
  (lambda (res-parcial lista) 
    (if (null? lista) res-parcial
        (if (= (car lista) 1) 1
            (mdc*-aux (mdc res-parcial (car lista))
                      (cdr lista))))))






(define id
  (lambda (x)
    x))

(define mdc-otimo*
  (lambda (lista)
    ;(displayln lista)
    ;(displayln id)
    (mdc*-aux2 lista id)))

(define mdc*-aux2
  (lambda (lista resto-da-conta)
    ;(displayln lista)
    ;(displayln resto-da-conta)
    (if (= (car lista) 1) 1
        (if (null? (cdr lista)) ; acabou a lista,calculemos
            (resto-da-conta (car lista))
            (mdc*-aux2 (cdr lista)
                       (lambda (n) (resto-da-conta (mdc (car lista)n))))))));^^^^^^^^^^->novo resto-da-conta

