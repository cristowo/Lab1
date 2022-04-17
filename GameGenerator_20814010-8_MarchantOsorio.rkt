#lang racket
(require "GenerarCards_20814010-8_MarchantOsorio.rkt")

;TDA - Game---------------------------------------------------------------------------------------------------------------
;tda->player (nombre turno puntos)

;TDA game - constructor---------------------------------------------------------------------------------------------------
;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo: 
(define (player n)
  (cond
    [(= n 0) null]
    [else (cons (list n 0 0) (player (- n 1)))]))

;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo: 
(define game (lambda (Np L Modo FR)
               (cond
                 [(equal? Np 0) null]
                 [else (cons (player Np) (Modo L))])))

;TDA game - stackMode----------------------------------------------------------------------------------------------------
;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo: 
(define (stackMode L)
  (cond
    [(empty? L) null]
    [(empty? (cdr L)) null]
    [else (list (list (encontrar L (length L)) (encontrar L (-(length L)1))) (eleminar (eleminar L (length L)) (-(length L)1)))]))

;TDA game - register-----------------------------------------------------------------------------------------------------
;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo: 
(define (insertarRegister L e)
  (cond
    [(empty? L) null]
    [(equal? (car(car L)) e) L]
    [(number? (car(car L))) (cons (list e 0 0) (cdr L))]
    [else (cons (car L) (insertarRegister (cdr L) e))]))

;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo:     
(define (register name L)
    (cons (insertarRegister (car L) name) (cdr L)))

;TDA game - whoseTurnIsIt?-----------------------------------------------------------------------------------------------
;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo: 
(define (turno? L L1)
  (cond
    [(empty? (cdr L)) (car(car L1))]
    [(equal? (car(cdr(car L))) (car(cdr(car(cdr L))))) (turno? (cdr L) L1)]
    [(> (car(cdr(car L)))  (car(cdr(car(cdr L))))) (car(car(cdr L)))]))

;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo:                  
(define (whoseTurnIsIt? L)
  (turno? (car L) (car L)))

;TDA game - play---------------------------------------------------------------------------------------------------------
;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo: 
(define (nulo L)
  L)
;spotlt-----------------------------------------------------------------------
;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo: 
(define (aumentTurno L) ;car car L
  (cond
    [(empty? L) null]
    [else (append(list (car L) (+ (car(cdr L)) 1)) (cdr(cdr L)))]))

;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo: 
(define (aumentPunt L) ;car  car L
  (cond
    [(empty? L) null]
    [else (list (car L) (car(cdr L)) (+(car(cdr(cdr L)))1))]))

;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo: 
(define (buscarSpotlt L n) ; L n
  (cond
    [(empty? L) #false]
    [(equal? (car L) n) #true]
    [else (buscarSpotlt (cdr L) n)]))

;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo: 
(define (GanoPuntos L) ; car L
  (cond
    [(equal? (car(car L))(turno? L L)) (cons (aumentPunt(aumentTurno (car L))) (cdr L))]
    [else (cons (car L) (GanoPuntos (cdr L)))]))

;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo: 
(define (Spotlt2 L n)
  (cond
    [(and(buscarSpotlt (car(car(cdr L))) n) (buscarSpotlt (car(cdr(car(cdr L)))) n)) (GanoPuntos (car L))]
    [else (pass1 (car L))]))

;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo: 
(define (SigTurn L)
  (list (list (encontrar L (length L)) (encontrar L (-(length L)1))) (eleminar (eleminar L (length L)) (-(length L)1))))

;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo: 
(define (Spotlt3 n L)
  (cons (Spotlt2  L n) (stackMode (car(cdr(cdr L))))))

;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo: 
(define (spotIt n)
  n)

;pass-------------------------------------------------------------------------
;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo: 
(define (pass1 L)
  (cond
    [(equal? (car(car L))(turno? L L)) (cons (aumentTurno (car L)) (cdr L))]
    [else (cons (car L) (pass1 (cdr L)))]))

;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo: 
(define (pass L)
  (cons (pass1 (car L)) (cdr L)))

;finish-------------------------------------------------------------------------
;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo: 
(define (encontFinish L e)           ;encuentra y elimina el elemento de la lista
  (cond
    [(empty? L) null]
    [(equal? (car(car L)) e) (cdr L)]
    [else (cons (car L) (encontFinish (cdr L) e))]))

;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo: 
(define (reordenar L e a)
  (cond
    [(empty? L) a]
    [(>= (car(cdr(cdr(car L)))) e) (reordenar (cdr L) (car(cdr(cdr(car L)))) (car(car L)))]
    [else (reordenar (cdr L) e a)]))

;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo: 
(define (finish1 L i)
  (cond
    [(empty? L) null]
    [else (cons (list (reordenar L 0 0) i) (finish1 (encontFinish  L (reordenar L 0 0)) (+ i 1)))]))

;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo: 
(define (finish L)
  (finish1 (car L) 1))
;Play----------------------------------------------------------------------------
;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo: 
(define play (lambda (L action)
               (cond
                 [(equal? action null) (nulo L)]
                 [(string? action)(Spotlt3 action L)]
                 [else (action L)])))

;TDA game - status-------------------------------------------------------------------------------------------------
;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo: 
(define (status L)
  L)

;TDA game - score--------------------------------------------------------------------------------------------------
;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo: 
(define (score1 L name)
  (cond
    [(empty? L) null]
    [(equal? (car(car L)) name) (car(cdr(cdr(car L))))]
    [else (score1 (cdr L) name)]))

;Funcion 
;Dominio: 
;Recorrido: 
;Descripcion: 
;Tipo: 
(define (score L name)
  (score1 (car L) name))
  
;TDA game - game->string-------------------------------------------------------------------------------------------






(define mazo (cardsSet ListS 8 -1 #t))
(define game1 (game 4 mazo stackMode random))
(define game2 (register "juanito" game1))
(define game3 (register "carlitos" game2))
(define game4 (register "manolito" game3))
(define game5 (register "pedrito" game4))

