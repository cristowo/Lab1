#lang racket
;--------Para Importar Funciones---------------------------------------------------------------------------------------
(require "TDA-game-play_20814010-8_MarchantOsorio.rkt")   ;para el ejemplo
;--------Para Exportar Funciones---------------------------------------------------------------------------------------
(provide (all-defined-out))
;----------------------------------------------------------------------------------------------------------------------

;----------------------------------TDA game - status-------------------------------------------------------------------
;Funcion status
;Dominio: Lista
;Recorrido: Lista
;Descripcion: Regresa el estado actual del juego
;Tipo: Declarativa
(define (status L)
  L)

;---------------------------------------EJEMPLOS------------------------------------------------------------------------
;usando los ejemplos de play
(define S1 (status gameplay3 )) ;status de la partida iniciando
(define S2 (status play5 ))     ;status de la partida a medias
(define S3 (status play7 ))     ;status del juego terminado