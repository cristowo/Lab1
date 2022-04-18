#lang racket
;--------Para Importar Funciones---------------------------------------------------------------------------------------
(require "TDA-game-whoseTurnIsIt_20814010-8_MarchantOsorio.rkt")   ;para funcion turno?
(require "TDA-cardsSet-constructor_20814010-8_MarchantOsorio.rkt") ;para funcion encontrar y eleminar
(require "TDA-game-stackMode_20814010-8_MarchantOsorio.rkt")
(require "TDA-game-register_20814010-8_MarchantOsorio.rkt")         ;solo para el ejemplo
(require "TDA-game-constructor_20814010-8_MarchantOsorio.rkt")      ;solo para el ejemplo
;--------Para Exportar Funciones---------------------------------------------------------------------------------------
(provide (all-defined-out))
;----------------------------------------------------------------------------------------------------------------------

;TDA game - play-------------------------------------------------------------------------------------------------------
;null------------------------------------------------------------------------------------------------------------------
;Funcion nulo
;Dominio: Lista
;Recorrido: Lista
;Descripcion: caso cuando la action de play es null, y regresa el estado del game
;Tipo: Declarativa
(define (nulo L)
  L)
;spotlt----------------------------------------------------------------------------------------------------------------
;Funcion aumentTurno 
;Dominio: Lista
;Recorrido: Lista
;Descripcion: en la lista de jugadores aumenta el turno en 1 para la persona que este de turno
;Tipo: Declarativa
(define (aumentTurno L) 
  (cond
    [(empty? L) null]
    [else (append(list (car L) (+ (car(cdr L)) 1)) (cdr(cdr L)))]))

;Funcion aumentPunt
;Dominio: Lista
;Recorrido: Lista
;Descripcion: en la lista de jugadores aumenta el puntaje en 1 para la persona que este de turno
;Tipo: Declarativa
(define (aumentPunt L) 
  (cond
    [(empty? L) null]
    [else (list (car L) (car(cdr L)) (+(car(cdr(cdr L)))1))]))

;Funcion buscarSpotlt
;Dominio: Lista X String
;Recorrido: Booleano
;Descripcion: Comprueba si el string esta dentro de las cartas volteadas
;Tipo: Cola
(define (buscarSpotlt L n) ; L n
  (cond
    [(empty? L) #false]
    [(equal? (car L) n) #true]
    [else (buscarSpotlt (cdr L) n)]))

;Funcion GanoPuntos
;Dominio: Lista
;Recorrido: Lista (jugadores)
;Descripcion: Si se ganaron puntos agrega puntos y suma un turno, sino solo suma un turno
;Tipo: Cola
(define (GanoPuntos L) ; car L
  (cond
    [(equal? (car(car L))(turno? L L)) (cons (aumentPunt(aumentTurno (car L))) (cdr L))]
    [else (cons (car L) (GanoPuntos (cdr L)))]))

;Funcion Spotlt2
;Dominio: Lista X String
;Recorrido: Lista
;Descripcion: Comprueba si se ganaron puntos, sino pasa el turno
;Tipo: Declarativa
(define (Spotlt2 L n)
  (cond
    [(and(buscarSpotlt (car(car(cdr L))) n) (buscarSpotlt (car(cdr(car(cdr L)))) n)) (GanoPuntos (car L))]
    [else (pass1 (car L))]))

;Funcion SigTurn
;Dominio: Lista
;Recorrido: Lista
;Descripcion: Elimina las cartas en juego, y agrega otras 2 cartas del mazo
;Tipo: Declarativa
(define (SigTurn L)
  (list (list (encontrar L (length L)) (encontrar L (-(length L)1))) (eleminar (eleminar L (length L)) (-(length L)1))))

;Funcion Spotlt3
;Dominio: String X Lista
;Recorrido: Lista (game)
;Descripcion: Se ingresa el game y el elemento en comun, y se hace el proceso de SpotIt
;Tipo: Declarativa
(define (Spotlt3 n L)
  (cons (Spotlt2  L n) (stackMode (car(cdr(cdr L))))))

;Funcion spotIt
;Dominio: String
;Recorrido: String
;Descripcion: Regresa el String ingresado
;Tipo: Declarativa
(define (spotIt n)
  n)

;pass---------------------------------------------------------------------------------------------------------------------
;Funcion pass1
;Dominio: Lista
;Recorrido: Lista(game)
;Descripcion: aumenta en uno la cantidad de turnos del jugador actual
;Tipo: 
(define (pass1 L)
  (cond
    [(equal? (car(car L))(turno? L L)) (cons (aumentTurno (car L)) (cdr L))]
    [else (cons (car L) (pass1 (cdr L)))]))

;Funcion pass
;Dominio: Lista
;Recorrido: Lista (game)
;Descripcion: junta el aumento del turno con el resto del game
;Tipo: Declarativa
(define (pass L)
  (cons (pass1 (car L)) (cdr L)))

;finish-------------------------------------------------------------------------------------------------------------------
;Funcion encontFinish
;Dominio: Lista X String
;Recorrido: Lista 
;Descripcion: encuentra y elimina el elemento de la lista
;Tipo: Natural
(define (encontFinish L e)          
  (cond
    [(empty? L) null]
    [(equal? (car(car L)) e) (cdr L)]
    [else (cons (car L) (encontFinish (cdr L) e))]))

;Funcion reordenar
;Dominio: Lista X Entero X Entero
;Recorrido: "String"
;Descripcion: Regresa el jugador con la mayor puntuación
;Tipo: Cola
(define (reordenar L e a)
  (cond
    [(empty? L) a]
    [(>= (car(cdr(cdr(car L)))) e) (reordenar (cdr L) (car(cdr(cdr(car L)))) (car(car L)))]
    [else (reordenar (cdr L) e a)]))

;Funcion finish1
;Dominio: Lista X Entero
;Recorrido: Lista
;Descripcion: Regresa a los jugadores por odern de puntaje 
;Tipo: Natural
(define (finish1 L i)            ;aclarar que el 1 significa primero y asi sucesivamente, es decir gana el 1
  (cond
    [(empty? L) null]
    [else (cons (list (reordenar L 0 0) i) (finish1 (encontFinish  L (reordenar L 0 0)) (+ i 1)))]))

;Funcion finish
;Dominio: Lista 
;Recorrido: Lista
;Descripcion: regresa la tabla de posiciones de los jugadores
;Tipo: Declarativa
(define (finish L)             ;aclarar que el 1 significa primero y asi sucesivamente, es decir gana el 1
  (finish1 (car L) 1))
;Play---------------------------------------------------------------------------------------------------------------------
;Funcion Play 
;Dominio: Lista X Funcion (o null)
;Recorrido: Lista (Game)
;Descripcion: funcion de jugar, realiza la accion segun la funcion establecida
;Tipo: Declarativa
(define play (lambda (L action)
               (cond
                 [(equal? action null) (nulo L)]
                 [(string? action)(Spotlt3 action L)]
                 [else (action L)])))

;--------------------------------------------EJEMPLOS----------------------------------------------------------------------
;definicion base-------------------------------------------
(define mazoplay (cardsSet ListS 4 -1 #f))
(define gameplay1 (game 2 mazoplay stackMode #f))
(define gameplay2 (register "juanito" gameplay1))
(define gameplay3 (register "manolito" gameplay2))
;ejemplos de uso------------------------------------------
(define play1(play gameplay3 null))
(define play2(play play1 pass))
(define play3(play play2 (spotIt "D ")))
(define play4(play play3 (spotIt "B ")))
(define play5(play play4 (spotIt "C ")))
(define play6(play play5 (spotIt "B ")))
(define play7(play play6 finish))          ;aclarar que el 1 significa primero y asi sucesivamente, es decir gana el 1