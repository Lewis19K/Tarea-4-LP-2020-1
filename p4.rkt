#lang racket

;;(crear-arbol)
;;crea un arbol (una lista vacia)
;;el arbol o lista que creo
(define crear-arbol '())


;;(value node)
;;para tomar el valor de la raiz del arbol que recibe (node), puede recibir sub-arboles
;;returna el valor
(define (value node)
  (if (null? node) '()
      (car node)))


;;(left node)
;;para tomar el subarbol del hijo izquierdo del arbol que recibe por el parametro node
;;returna el subarbol que tiene como raiz el hijo izquierdo
(define (left node)
  (if (null? node) '()
      (cadr node)))


;;(right node)
;;para tomar el subarbol del hijo derecho del arbol que recibe por el parametro node
;;returna el subarbol que tiene como raiz el hijo derecho
(define (right node)
  (if (null? node) '()
      (caddr node)))


;;(agregar nodo entero)
;;agrega el entero ("entero") al arbol
;;retorna el arbol despues de agregar el numero
(define (agregarr nodo entero)
  (cond
    ((null? nodo)
     (list entero '() '()))
    (else
     (cond
       ((< entero (value nodo))
        (cons (value nodo) (list (agregarr (left nodo) entero) (right nodo) )))
       (else
        (cons (value nodo) (list (left nodo) (agregarr (right nodo) entero) )))))))


;;(mostrar arbol)
;;imprime el arbol que recibe por parametro
;;no tiene un return, solo imprime
(define (mostrar arbol)
  (displayln arbol))


;;(estructurada? lista)
;;para saber si esta lista tiene listas dentro de ella
;;retorna #t si es que tiene sublistas como elemento, o retorna #f si no tiene sublistas
(define (estructurada? lista)
   (and (not (null? lista))
        (or (list? (car lista))
            (estructurada? (cdr lista)))))


;;(recorrlista lista numero)
;;ve si en esta lista esta el numero que se busca
;;return #t si es que esta, #f si no esta
(define (recorrlista lista numero)
  (cond
    ((null? lista) #f)
    ((number? (car lista))
     (cond
       ((equal? numero (car lista)) #t)
       ((< numero (car lista))
        (recorrlista (left lista) numero))
       ((> numero (car lista))
        (recorrlista (right lista) numero))
       (else #f)))
    (else
     (cond
       ((estructurada? (car lista))
        (recorrlista (car lista) numero))
       ))))




;;aqui creo un arbol como variable global, el cual modificare con el tda
(define arbol crear-arbol)

(define (abb string entero)
  (cond
    ((eqv? string 'agregar)
     (set! arbol (agregarr arbol entero)))
    ((eqv? string 'mostrar)
     (mostrar arbol))
    ((eqv? string 'buscar)
     (recorrlista arbol entero))
    (else
     displayln "no se ingreso una funcion del tda valida")))