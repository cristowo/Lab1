#lang racket
;--------Para Importar Funciones---------------------------------------------------------------------------------------
(require "TDA-cardsSet-constructor_20814010-8_MarchantOsorio.rkt")
(require "TDA-cardsSet-nthCard_20814010-8_MarchantOsorio.rkt")
;--------Para Exportar Funciones---------------------------------------------------------------------------------------
(provide (all-defined-out))
;----------------------------------------------------------------------------------------------------------------------

;--------------------------------- TDA cardsSet - findTotalCards-------------------------------------------------------
;Funcion findTotalCards
;Dominio: Lista
;Recorrido: Entero
;Descripcion: A partir de una carta de muestra, determina la cantidad total de cartas que se deben producir para construir un conjunto válido.
;Tipo: Declarativa
(define (findTotalCards L)
  (+(*(- (length L )1)(- (length L)1))(length L)))

;----------------------------------Ejemplo-----------------------------------------------------------------------------
(define cardFTC1 (findTotalCards (nthCard(cardsSet ListS 8 -1 #true) 15))) ;findTotalCards -> carta 15 de un mazo de 8 elementos con roden aleatorio
(define cardFTC2 (findTotalCards (nthCard(cardsSet ListS 5 -1 #false) 5))) ;findTotalCards -> carta 5 de un mazo de 3 elementos 
(define cardFTC3 (findTotalCards (nthCard(cardsSet ListS 5 6 #true) 3)))   ;findTotalCards -> carta 3 mazo de 5 elementos y maximo 6 cartas con orden aleatorio