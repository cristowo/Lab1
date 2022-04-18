#lang racket
;--------Para Importar Funciones---------------------------------------------------------------------------------------
(require "TDA-cardsSet-constructor_20814010-8_MarchantOsorio.rkt")
;--------Para Exportar Funciones---------------------------------------------------------------------------------------
(provide (all-defined-out))
;----------------------------------------------------------------------------------------------------------------------

;--------------------------------- TDA cardsSet - CardsSet->String  ---------------------------------------------------
;Funcion CardsSet->String2
;Dominio: Lista 
;Recorrido: Lista
;Descripcion: transforma cada carta en un string
;Tipo: Natural
(define (CardsSet->String2 L)
  (cond
    [(empty? L) null]
    [else (cons (apply string-append (car L))(CardsSet->String2 (cdr L)))]))

;Funcion putNCard
;Dominio: Lista X Entero
;Recorrido: Lista
;Descripcion: agrega elementos para el aspecto de cada carta
;Tipo: Natural
(define (putNCard L i)
  (cond
    [(empty? L) null]
    [else (cons(append (list "Card " (number->string  i) ": " (car L)) (list "\n")) (putNCard (cdr L) (+ i 1)))]))

;Funcion CardsSet->String
;Dominio: Lista
;Recorrido: String
;Descripcion: transforma la Lista de Strings genera anteriormente en un String
;Tipo: Declarativa
(define (CardsSet->String L)
  (apply string-append(CardsSet->String2 (putNCard (CardsSet->String2 L) 1))))

;-------------------------------EJEMPLOS-----------------------------------------------------------------------------
(define CS->S1(CardsSet->String(cardsSet ListS 4 -1 #true)))
;(display  CS->S1)
(define CS->S2(CardsSet->String(cardsSet ListS 4 -1 #false)))
;(display  CS->S2)
(define CS->S3(CardsSet->String(cardsSet ListS 3 4 #true)))
;(display  CS->S3)