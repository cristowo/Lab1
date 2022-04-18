#lang racket
;--------Para Importar Funciones---------------------------------------------------------------------------------------
(require "TDA-cardsSet-constructor_20814010-8_MarchantOsorio.rkt")
;--------Para Exportar Funciones---------------------------------------------------------------------------------------
(provide (all-defined-out))
;----------------------------------------------------------------------------------------------------------------------

;--------------------------TDA cardsSet - numCards -----------------------------------------------------------------
;Funcion loopNc
;Dominio: Lista X Entero
;Recorrido: Entero
;Descripcion: Cuenta la cantidad de elementos en una lista (preferi generar mi popio length)
;Tipo: Cola
(define (loopNc L i)
  (cond
    [(empty? L) i]
    [else (loopNc (cdr L)(+ i 1))]))

;Funcion numCards
;Dominio: Lista
;Recorrido: Entero
;Descripcion: Regresa la cantidad de elementos en una lista
;Tipo: Declarativa
(define (numCards L)
  (loopNc L 0))

;---------------------------EJEMPLOS--------------------------------------------------------------------------
(define numerocartas1 (numCards CARTA1))                       ;numCards -> mazo usado en cartSet-Constructor
(define numerocartas2 (numCards (cardsSet ListS 3 4 #false)))  ;numCards -> mazo de 3 elementos y 4 cartas
(define numerocartas3 (numCards (cardsSet ListS 8 -1 #true)))  ;numCards -> mazo de 8 elementos y orden al azar
