#lang racket
;--------Para Importar Funciones---------------------------------------------------------------------------------------
(require "TDA-cardsSet-constructor_20814010-8_MarchantOsorio.rkt")
;--------Para Exportar Funciones---------------------------------------------------------------------------------------
(provide (all-defined-out))
;----------------------------------------------------------------------------------------------------------------------

;---------------------------------- TDA cardsSet - Dobble?-------------------------------------------------------------
;Funcion buscar
;Dominio: Lista X Lista X Lista X Entero
;Recorrido: Entero
;Descripcion entrega el numero de elementos repetidos que hay entre ambas listas.
;Tipo: Cola
(define (buscar L1 L2 L12 acum) ;ej: (buscar (list 1 2 3 3 5 3 6) (list 1 2 3 4 5 6) (list 1 2 3 3 5 3 6) 0)
  (cond
    [(empty? L1) (buscar L12 (cdr L2) L12 acum)]
    [(empty? L2) acum]
    [(equal? (car L1) (car L2)) (buscar(cdr L1) L2 L12 (+ acum 1))]
    [else (buscar(cdr L1) L2 L12 acum)]))

;Funcion buscar3
;Dominio: Lista X Lista X Lista
;Recorrido: Booleano
;Descripcion recorre ambas listas y las envia a buscar. Solo 1 elemento en comun, de esta forma ve que el n-1 sea primo o un numero elevado a un primo.
;Tipo: Cola
(define (buscar3 L1 L2 L22)
  (cond
    [(empty? (cdr L1)) #true]
    [(empty? (cdr L2)) (buscar3 (cdr L1) (cdr L22) (cdr L22))]
    [(= (buscar (car L1) (car(cdr L2)) (car L1) 0) 1) (buscar3 L1 (cdr L2) L22)]
    [else #false]))


;Funcion buscar2
;Dominio: Lista X Lista
;Recorrido: Booleano
;Descripcion: no elentos repetido en una misma carta.
;Tipo: Cola
(define (buscar2 L1 L2)
  (cond
    [(empty? L1) #true]
    [(= (buscar (car L1) (car L2) (car L1) 0) (length (car L1))) (buscar2 (cdr L1) (cdr L2))]
    [else #false]))

;Funcion buscar3
;Dominio: Listas
;Recorrido: Booleano
;Descripcion: misma cantidad de elementos.
;Tipo:Cola

(define (buscar4 L)
  (cond
    [(empty? (cdr L)) #true]
    [(= (length (car L)) (length (car(cdr L)))) (buscar4 (cdr L))]
    [else #false]))

;Funcion Dobble?
;Dominio: Lista
;Recorrido: Booleano
;Descripcion: Recopilacion de anteriores y define si es un conjunto valido o no
;Tipo: Declarativa
(define (Dobble? L)
  (cond
    [(buscar4 L)(cond
                  [(buscar2 L L) (cond
                                   [(buscar3 L L L) #true]
                                   [else #false])]
                  [else #false])]
    [else #false]))

;-----------------------------------Ejemplos--------------------------------------------------------
(define IsDobble?1 (Dobble?(cardsSet ListS 7 -1 #true)))   ;Dobble? -> mazo de 7 elementos
(define IsDobble?2 (Dobble?(cardsSet ListS 7 13 #false)))  ;Dobble? -> mazo de 7 elementos y 13 cartas
(define IsDobble?3 (Dobble?(cardsSet ListS 3 -1 #true)))   ;Dobble? -> mazo de 3 elementos