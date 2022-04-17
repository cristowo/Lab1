#lang racket
;--------Para Importar Funciones---------------------------------------------------------------------------------------
(require "TDA-cardsSet-constructor_20814010-8_MarchantOsorio.rkt")
(require "TDA-cardsSet-Dobble_20814010-8_MarchantOsorio.rkt")
(require "TDA-cardsSet-numCards_20814010-8_MarchantOsorio.rkt")
;--------Para Exportar Funciones---------------------------------------------------------------------------------------
(provide (all-defined-out))
;----------------------------------------------------------------------------------------------------------------------

;--------------------------- TDA cardsSet - missingCards  -------------------------------------------------------------
;Funcion RecoMiss
;Dominio: Lista X Lista X Lista
;Recorrido: Listas de listas
;Descripcion: regresa la cantidad de cartas para que el conjunto sea valido
;Tipo: Natural
(define (RecoMiss L1 L2 L22)
  (cond
    [(empty? L1) null]                                              ;cuando la lista se acaba 
    [(empty? L2) (cons (car L1) (RecoMiss (cdr L1) L22 L22))]       ;cuando el elemento se encontro se agrega a lista
    [(equal? (car L1)(car L2))(RecoMiss (cdr L1) L22 L22)]          ;cuando el elemento si esta en la lista se descarta y sigue
    [else (RecoMiss L1 (cdr L2) L22)]))                             ;caso de avance al siguiente elemento de la lista

;Funcion missingCards L
;Dominio: Lista (cartas)
;Recorrido: Lista de Listas
;Descripcion: a base de un conjunto de cartas, te entrega el resto de estas 
;Tipo: Declarativa
(define (missingCards L)
  (cond
    [(>= (buscar (car(cardsSet ListN (numCards (car L)) -1 #f)) (car L) (car(cardsSet ListN (numCards (car L)) -1 #f)) 0) 1) (RecoMiss(cardsSet ListN (numCards (car L)) -1 #f) L L)]
    [else (RecoMiss(cardsSet ListS (numCards (car L)) -1 #f) L L)]))

;----------------------------------------EJEMPLOS-----------------------------------------------------------------------
(define cardRM1 (RecoMiss (cardsSet ListS 3 -1 #t) (cardsSet ListS 3 4 #t) (cardsSet ListS 3 4 #t))) ;missingCards -> mazo de 3 elementos con orden alearotio y un mazo de 3 elementos maximo 4 cartas y orden aleatorio
(define cardMC1 (missingCards (cardsSet ListS 3 4 #t)))                                              ;missingCards -> mazo de 3 elementos, maximo 4 cartas y orden aleatorio 
(define cardMC2 (missingCards (cardsSet ListS 8 44 #t)))                                             ;missingCards -> mazo de 8 elementos, maximo 44 cartas y orden aleatorio 
(define cardMC3 (missingCards (cardsSet ListS 3 -1 #t)))                                             ;missingCards -> mazo de 3 elementos y orden aleatorio (Esta seria nulo, ya que no faltan cartas)