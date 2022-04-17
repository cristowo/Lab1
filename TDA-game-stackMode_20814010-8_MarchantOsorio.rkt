#lang racket
;--------Para Importar Funciones---------------------------------------------------------------------------------------
(require "TDA-cardsSet-constructor_20814010-8_MarchantOsorio.rkt")
;--------Para Exportar Funciones---------------------------------------------------------------------------------------
(provide (all-defined-out))
;----------------------------------------------------------------------------------------------------------------------
;-------------------------------------DECLARACION----------------------------------------------------------------------
;stackMod->(Cartas-en-Mesa Mazo)
;--------------------------------TDA game - stackMode------------------------------------------------------------------
;Funcion stackMode
;Dominio: Lista
;Recorrido: Listas
;Descripcion: genera el juego de stack mode, una lista con 2 cartas y el resto del mazo.
;Tipo: Declarativa
(define (stackMode L)
  (cond
    [(empty? L) null]
    [(empty? (cdr L)) null]
    [else (list (list (encontrar L (length L)) (encontrar L (-(length L)1))) (eleminar (eleminar L (length L)) (-(length L)1)))]))

;---------------------------------EJEMPLOS-------------------------------------------------------------------------------
(define STM1(stackMode(cardsSet ListS 4 -1 #t)))