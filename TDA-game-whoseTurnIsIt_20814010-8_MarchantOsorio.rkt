#lang racket
;--------Para Importar Funciones---------------------------------------------------------------------------------------
(require "TDA-game-register_20814010-8_MarchantOsorio.rkt")        ;solo para ejemplo
;--------Para Exportar Funciones---------------------------------------------------------------------------------------
(provide (all-defined-out))
;----------------------------------------------------------------------------------------------------------------------

;------------------------------TDA game - whoseTurnIsIt?---------------------------------------------------------------
;Funcion turno?
;Dominio: Lista X Lista
;Recorrido: String
;Descripcion: dice de quien es el turno, bajo la logica si todos estan iguales el turno es para el primero
              ;en otro caso si el anterior es mayor que el siguiente, el turno es para el siguiente.
;Tipo: Cola
(define (turno? L L1)
  (cond
    [(empty? (cdr L)) (car(car L1))] ;cuando son todos iguales regresa el primero de la lista
    [(equal? (car(cdr(car L))) (car(cdr(car(cdr L))))) (turno? (cdr L) L1)] ;caso cuando evalua igualdad
    [(> (car(cdr(car L)))  (car(cdr(car(cdr L))))) (car(car(cdr L)))])) ;caso cuando unos es mayor que el otro

;Funcion whoseTurnIsIt?
;Dominio: Lista
;Recorrido: String
;Descripcion: Evalua la lista en el turno? y regresa un nombre
;Tipo: Declarativa                 
(define (whoseTurnIsIt? L)
  (turno? (car L) (car L)))

;----------------------------------------EJEMPLOS----------------------------------------------------------------------
(define WTII1(whoseTurnIsIt? gameE4)) ;no puede haber otro ejemplo, faltaria la definicion de play