#lang racket
;--------Para Importar Funciones---------------------------------------------------------------------------------------
(require "TDA-cardsSet-constructor_20814010-8_MarchantOsorio.rkt")
(require "TDA-game-stackMode_20814010-8_MarchantOsorio.rkt")
;--------Para Exportar Funciones---------------------------------------------------------------------------------------
(provide (all-defined-out))
;----------------------------------------------------------------------------------------------------------------------

;--------------------------------------TDA - Game----------------------------------------------------------------------
;tda->player (nombre turno puntos)
;tda->game (player Cartas-en-Mesa Mazo)
;--------------------------------TDA game - constructor----------------------------------------------------------------
;Funcion player
;Dominio: Entero
;Recorrido:  Lista
;Descripcion: definine la estructura para cada jugador
;Tipo: Cola
(define (player n)
  (cond
    [(= n 0) null]
    [else (cons (list n 0 0) (player (- n 1)))]))

;Funcion game
;Dominio: Entero X Lista X Funcion X Booleano
;Recorrido: Lista de Listas
;Descripcion: genera el juego con el numero de jugadores, el mazo nuevo, el modo y si se quiere azar o no.
;Tipo: 
(define game (lambda (Np L Modo FR)
               (cond
                 [(equal? Np 0) null]
                 [else (cons (player Np) (Modo (cardsSet ListS (length(car L)) (length L) FR)))])))

;---------------------------------EJEMPLOS------------------------------------------------------------------------------
(define mazo(cardsSet ListS 5 -1 #f))

(define game1 (game 3 mazo stackMode #t))
(define game2 (game 2 mazo stackMode #f))
(define game3 (game 5 mazo stackMode #t))
