#lang racket
;--------Para Importar Funciones---------------------------------------------------------------------------------------
(require "TDA-game-play_20814010-8_MarchantOsorio.rkt")   ;para el ejemplo
;--------Para Exportar Funciones---------------------------------------------------------------------------------------
(provide (all-defined-out))
;----------------------------------------------------------------------------------------------------------------------

;---------------------------------TDA game - score---------------------------------------------------------------------
;Funcion score1
;Dominio: Lista X String
;Recorrido: Entero
;Descripcion: Regresa el puntaje de un jugador seleccionado
;Tipo: Cola
(define (score1 L name)
  (cond
    [(empty? L) null]
    [(equal? (car(car L)) name) (car(cdr(cdr(car L))))]
    [else (score1 (cdr L) name)]))

;Funcion score
;Dominio: Lista X String
;Recorrido: Entero
;Descripcion: Llama a la funcion anterior para encontrar el puntaje de cierto jugador
;Tipo: Declarativa
(define (score L name)
  (score1 (car L) name))

;-----------------------------------EJEMPLO---------------------------------------------------------------------------
(define Punt3(score play3 "manolito"))     ;usando los ejemplos de play
(define Punt1(score play5 "juanito"))      ;usando los ejemplos de play
(define Punt2(score play5 "manolito"))     ;usando los ejemplos de play
