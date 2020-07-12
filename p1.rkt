#lang racket

(define (tipos l1)
  (if (null? l1)
      null
      (if (integer? (car l1))
          (cons 'E
                (tipos (cdr l1)))
          (if (real? (car l1))
              (cons 'R
                    (tipos (cdr l1)))
              (if (complex? (car l1))
                  (cons 'C
                    (tipos (cdr l1)))
                  (display "Error"))))))