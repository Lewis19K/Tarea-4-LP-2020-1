#lang racket

(define (merge l1 l2)
  (define largo (+ (length l1) (length l2)))
  (define aux 0)
  (do ([i largo (- i 1)]
       [listaa (list) (append listaa (list aux))])
    ((zero? i) listaa)
    (cond
      ((null? l1)
       (cond
         ((null? l2)
          (set! aux null))
         (else
          (set! aux (car l2))
          (set! l2 (cdr l2)))))
      (else
       (cond
         ((null? l2)
          (set! aux (car l1))
          (set! l1 (cdr l1)))
         (else
          (cond
            ((< (car l1) (car l2))
             (set! aux (car l1))
             (set! l1 (cdr l1)))
            (else
             (set! aux (car l2))
             (set! l2 (cdr l2))))))))
    ))