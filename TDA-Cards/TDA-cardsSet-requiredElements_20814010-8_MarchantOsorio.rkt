#lang racket
;--------Para Importar Funciones---------------------------------------------------------------------------------------
(require "TDA-cardsSet-constructor_20814010-8_MarchantOsorio.rkt")
(require "TDA-cardsSet-nthCard_20814010-8_MarchantOsorio.rkt")
(require "TDA-cardsSet-findTotalCards_20814010-8_MarchantOsorio.rkt")
;--------Para Exportar Funciones---------------------------------------------------------------------------------------
(provide (all-defined-out))
;----------------------------------------------------------------------------------------------------------------------

;--------------------------- TDA cardsSet - requiredElements ----------------------------------------------------------
;Funcion requiredElements
;Dominio: Lista
;Recorrido: Entero
;Descripcion: A partir de una carta de muestra, determina la cantidad total de elementos para un mazo 
;Tipo: Declarativa
(define (requiredElements L)
  (findTotalCards L))

;-----------------------------EJEMPLOS---------------------------------------------------------------------------------
(define cardRE1 (requiredElements(nthCard(cardsSet ListS 3 -1 #true) 2)))      ;requiredElements -> carta 2 de un mazo de 3 elementos con orden aleatorio 
(define cardRE2 (requiredElements(nthCard(cardsSet ListS 4 -1 #false) 5)))     ;requiredElements -> carta 5 de un mazo de 4 elementos
(define cardRE3 (requiredElements(nthCard(cardsSet ListS 6 6 #true) 1)))       ;requiredElements -> carta 1 de un mazo de 6 elementos con maximo 6 cartas de orden aleatorio