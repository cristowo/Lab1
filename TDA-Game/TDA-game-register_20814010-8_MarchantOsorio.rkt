#lang racket
;--------Para Importar Funciones---------------------------------------------------------------------------------------
(require "TDA-cardsSet-constructor_20814010-8_MarchantOsorio.rkt") ;solo para ejemplo
(require "TDA-game-constructor_20814010-8_MarchantOsorio.rkt")     ;solo para ejemplo
(require "TDA-game-stackMode_20814010-8_MarchantOsorio.rkt")       ;solo para ejemplo
;--------Para Exportar Funciones---------------------------------------------------------------------------------------
(provide (all-defined-out))
;----------------------------------------------------------------------------------------------------------------------

;------------------------------------TDA game - register---------------------------------------------------------------
;Funcion insertarRegister
;Dominio: Lista X String
;Recorrido: Lista
;Descripcion: Agrega el nuevo nombre a la lista del game
;Tipo: Natural
(define (insertarRegister L e)
  (cond
    [(empty? L) null]
    [(equal? (car(car L)) e) L]
    [(number? (car(car L))) (cons (list e 0 0) (cdr L))]
    [else (cons (car L) (insertarRegister (cdr L) e))]))

;Funcion register
;Dominio: String X Lista
;Recorrido: Lista de Listas
;Descripcion: agrega la lista con los nuevos nombres a el game
;Tipo: Declarativa     
(define (register name L)
    (cons (insertarRegister (car L) name) (cdr L)))

;----------------------------------EJEMPLOS----------------------------------------------------------------------------
(define mazo1 (cardsSet ListS 8 -1 #f))
(define gameE1 (game 3 mazo1 stackMode #t))
(define gameE2 (register "juanito" gameE1))
(define gameE3 (register "carlitos" gameE2))
(define gameE4 (register "manolito" gameE3))