#lang scheme
(define combine-continuation (lambda (fu fb zero pred lista)
                               (cc-aux fu fb zero pred lista id)))


(define cc-aux (lambda (fu fb zero pred lista cont)
                 (if (null? lista) (cont zero)
                     (if (pred (car l)) zero
                         (cc-aux fu fb zero pred (cdr l) (lambda (x) (cont (fb (fu (car l)) x))))))))

;normal
; (if (null l) zero
;     (fb (fu (car l)) (combine fb fu zero (cdr l))))

(define combine-continuation (lambda (fb fu zero lista pred)
                               (lambda (lista)
                                 (call/cc (lambda (exit)
                                            (letrec (cc-aux lambda(fb fu zero lista pred)
                                                            (if (null? lista) zero
                                                                (if (pred (car l)) (exit zero)
                                                                    (fb (fu (car l)) (cc-aux fb fu zero (cdr l) pred)))))))))))