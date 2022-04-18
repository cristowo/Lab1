#lang racket
;--------Para Importar Funciones---------------------------------------------------------------------------------------
(require "TDA-cardsSet-constructor_20814010-8_MarchantOsorio.rkt")
(require "TDA-cardsSet-Dobble_20814010-8_MarchantOsorio.rkt")
;--------Para Exportar Funciones---------------------------------------------------------------------------------------
(provide (all-defined-out))
;----------------------------------------------------------------------------------------------------------------------

;------------------------------ TDA cardsSet - addCard ----------------------------------------------------------------
;Funcion addCard2
;Dominio: Lista X Lista
;Recorrido: Lista de Listas
;Descripcion: agrega una carta a la lista
;Tipo: Natural
(define (addCard2 L c)
  (cond
    [(empty? L) (cons c null)]
    [else (cons (car L) (addCard2 (cdr L) c))]))

;Funcion addCard
;Dominio: Lista X Lista
;Recorrido: Lista de Listas
;Descripcion: Comprueba si el set con la carta nueva es valido, si lo es la agrega y si no lo es no la agrega
;Tipo: Declarativa
(define (addCard L c)
  (cond
    [(Dobble? (addCard2 L c)) (addCard2 L c)]
    [else L]))

;-----------------------------EJEMPLOS---------------------------------------------------------------------------------
(define ADD1 (addCard (cardsSet ListS 3 4 #false) (list "B " "E " "G "))) ;adicion a un cardsSet
(define ADD2 (addCard null (list "B " "E " "G ")))                        ;adicion a lista vacia
(define ADD3 (addCard ADD2 (list "B " "E " "G ")))                        ;adicion a lista con el mismo elemento (no agrega nada)
(define ADD4 (addCard ADD3 (list "A " "E " "D ")))                        ;adcion a lista con un elemento