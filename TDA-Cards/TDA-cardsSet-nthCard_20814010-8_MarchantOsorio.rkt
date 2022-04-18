#lang racket
;--------Para Importar Funciones---------------------------------------------------------------------------------------
(require "TDA-cardsSet-constructor_20814010-8_MarchantOsorio.rkt")
;--------Para Exportar Funciones---------------------------------------------------------------------------------------
(provide (all-defined-out))
;----------------------------------------------------------------------------------------------------------------------

;------------------------------TDA cardsSet - nthCard -----------------------------------------------------------------
;Funcion loopNth
;Dominio: Lista X Entero X Entero
;Recorrido: Elemento (o una Lista)
;Descripcion: regresa el elemento que se encuentre en sierta posición
;Tipo: Cola
(define (loopNth L j i)
  (cond
    [(empty? L) null]                      ;Caso lista vacia o fuera del alcance
    [(= (- j 1) i) (car L)]                ;Devuelve la carta solicitada
    [else (loopNth (rest L) j (+ i 1))]))  ;Cumple el loop sumando 1 a i

;Funcion nthCard
;Dominio: Lista X Entero 
;Recorrido: Lista
;Descripcion: regresa la carta que se encuentre en la posicion j
;Tipo: Declarativa
(define (nthCard L j)                      ;Funcion principal nthCard, L es lista; j el numero
  (loopNth L j 0))                         ;Envia los datos al loop

;------------------------------Ejemplos--------------------------------------------------------------------------------
(define CardNTH1 (nthCard (cardsSet ListS 3 -1 #false) 2))     ;nthCard -> mazo de 3 elementos 
(define CardNTH2 (nthCard (cardsSet ListS 3 -1 #true) 2))      ;nthCard -> mazo de 3 elementos con orden aleatorio
(define CardNTH3 (nthCard (cardsSet ListS 3 4 #false) 2))      ;nthCard -> mazo de 3 elementos y max 4 cartas