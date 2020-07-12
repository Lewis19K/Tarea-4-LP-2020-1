#lang racket
;;------------------recursion simple----------------------
(define (periS lados)
  (if (not (null? lados))
      (+ (car lados) (periS (cdr lados)))
      0))
;;------------------recursion de cola---------------------
(define (periC lados)
  (let sumaLados ((listaa lados) (i 0))
    (if (not (null? listaa))
        (sumaLados(cdr listaa)(+ i (car listaa)))
        i)))