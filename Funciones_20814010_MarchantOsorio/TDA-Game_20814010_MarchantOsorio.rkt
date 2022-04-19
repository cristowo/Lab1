#lang racket
;--------Para Importar Funciones---------------------------------------------------------------------------------------
(require "TDA-CardsSet_20814010_MarchantOsorio.rkt")
;--------Para Exportar Funciones---------------------------------------------------------------------------------------
(provide (all-defined-out))
;----------------------------------------------------------------------------------------------------------------------

;----------------------------------------------------------------------------------------------------------------------
;--------------------------------TDA game - constructor----------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------

;tda->player (nombre turno puntos)
;tda->game (player Cartas-en-Mesa Mazo)

;Funcion Constructor
;Dominio: Entero
;Recorrido:  Lista
;Descripcion: definine la estructura para cada jugador
;Tipo: Cola
(define (player n)
  (cond
    [(= n 0) null]
    [else (cons (list n 0 0) (player (- n 1)))]))

;Funcion Constructor
;Dominio: Entero X Lista X Funcion X Booleano
;Recorrido: Lista de Listas
;Descripcion: genera el juego con el numero de jugadores, el mazo nuevo, el modo y si se quiere azar o no.
;Tipo: 
(define game (lambda (Np L Modo FR)
               (cond
                 [(equal? Np 0) null]
                 [else (cons (player Np) (Modo (cardsSet ListS (length(car L)) (length L) FR)))])))

;----------------------------------------------------------------------------------------------------------------------
;--------------------------------TDA game - stackMode------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------

;-------------------------------------DECLARACION----------------------------------------------------------------------
;stackMod->(Cartas-en-Mesa Mazo)
;----------------------------------------------------------------------------------------------------------------------

;Funcion Constructor
;Dominio: Lista
;Recorrido: Listas
;Descripcion: genera el juego de stack mode, una lista con 2 cartas y el resto del mazo.
;Tipo: Declarativa
(define (stackMode L)
  (cond
    [(empty? L) null]
    [(empty? (cdr L)) null]
    [else (list (list (encontrar L (length L)) (encontrar L (-(length L)1))) (eleminar (eleminar L (length L)) (-(length L)1)))]))

;----------------------------------------------------------------------------------------------------------------------
;------------------------------------TDA game - register---------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------

;Funcion Modificador
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

;Funcion Modificador
;Dominio: String X Lista
;Recorrido: Lista de Listas
;Descripcion: agrega la lista con los nuevos nombres a el game
;Tipo: Declarativa     
(define (register name L)
    (cons (insertarRegister (car L) name) (cdr L)))

;----------------------------------------------------------------------------------------------------------------------
;------------------------------TDA game - whoseTurnIsIt?---------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------
;Funcion Selector
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

;Funcion Selector
;Dominio: Lista
;Recorrido: String
;Descripcion: Evalua la lista en el turno? y regresa un nombre
;Tipo: Declarativa                 
(define (whoseTurnIsIt? L)
  (turno? (car L) (car L)))

;----------------------------------------------------------------------------------------------------------------------
;----------------------------------TDA game - play---------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------
;null------------------------------------------------------------------------------------------------------------------
;Funcion Selector
;Dominio: Lista
;Recorrido: Lista
;Descripcion: caso cuando la action de play es null, y regresa el estado del game
;Tipo: Declarativa
(define (nulo L)
  L)
;spotlt----------------------------------------------------------------------------------------------------------------
;Funcion Modificador
;Dominio: Lista
;Recorrido: Lista
;Descripcion: en la lista de jugadores aumenta el turno en 1 para la persona que este de turno
;Tipo: Declarativa
(define (aumentTurno L) 
  (cond
    [(empty? L) null]
    [else (append(list (car L) (+ (car(cdr L)) 1)) (cdr(cdr L)))]))

;Funcion Modificador
;Dominio: Lista
;Recorrido: Lista
;Descripcion: en la lista de jugadores aumenta el puntaje en 1 para la persona que este de turno
;Tipo: Declarativa
(define (aumentPunt L) 
  (cond
    [(empty? L) null]
    [else (list (car L) (car(cdr L)) (+(car(cdr(cdr L)))1))]))

;Funcion Pertinencia 
;Dominio: Lista X String
;Recorrido: Booleano
;Descripcion: Comprueba si el string esta dentro de las cartas volteadas
;Tipo: Cola
(define (buscarSpotlt L n) ; L n
  (cond
    [(empty? L) #false]
    [(equal? (car L) n) #true]
    [else (buscarSpotlt (cdr L) n)]))

;Funcion Modificador
;Dominio: Lista
;Recorrido: Lista (jugadores)
;Descripcion: Si se ganaron puntos agrega puntos y suma un turno, sino solo suma un turno
;Tipo: Cola
(define (GanoPuntos L) ; car L
  (cond
    [(equal? (car(car L))(turno? L L)) (cons (aumentPunt(aumentTurno (car L))) (cdr L))]
    [else (cons (car L) (GanoPuntos (cdr L)))]))

;Funcion Modificador
;Dominio: Lista X String
;Recorrido: Lista
;Descripcion: Comprueba si se ganaron puntos, sino pasa el turno
;Tipo: Declarativa
(define (Spotlt2 L n)
  (cond
    [(and(buscarSpotlt (car(car(cdr L))) n) (buscarSpotlt (car(cdr(car(cdr L)))) n)) (GanoPuntos (car L))]
    [else (pass1 (car L))]))

;Funcion Modificador
;Dominio: Lista
;Recorrido: Lista
;Descripcion: Elimina las cartas en juego, y agrega otras 2 cartas del mazo
;Tipo: Declarativa
(define (SigTurn L)
  (list (list (encontrar L (length L)) (encontrar L (-(length L)1))) (eleminar (eleminar L (length L)) (-(length L)1))))

;Funcion Modificador
;Dominio: String X Lista
;Recorrido: Lista (game)
;Descripcion: Se ingresa el game y el elemento en comun, y se hace el proceso de SpotIt
;Tipo: Declarativa
(define (Spotlt3 n L)
  (cons (Spotlt2  L n) (stackMode (car(cdr(cdr L))))))

;Funcion Selector
;Dominio: String
;Recorrido: String
;Descripcion: Regresa el String ingresado
;Tipo: Declarativa
(define (spotIt n)
  n)

;pass---------------------------------------------------------------------------------------------------------------------
;Funcion Modificador
;Dominio: Lista
;Recorrido: Lista(game)
;Descripcion: aumenta en uno la cantidad de turnos del jugador actual
;Tipo: 
(define (pass1 L)
  (cond
    [(equal? (car(car L))(turno? L L)) (cons (aumentTurno (car L)) (cdr L))]
    [else (cons (car L) (pass1 (cdr L)))]))

;Funcion Modificador
;Dominio: Lista
;Recorrido: Lista (game)
;Descripcion: junta el aumento del turno con el resto del game
;Tipo: Declarativa
(define (pass L)
  (cons (pass1 (car L)) (cdr L)))

;finish-------------------------------------------------------------------------------------------------------------------
;Funcion Modificador
;Dominio: Lista X String
;Recorrido: Lista 
;Descripcion: encuentra y elimina el elemento de la lista
;Tipo: Natural
(define (encontFinish L e)          
  (cond
    [(empty? L) null]
    [(equal? (car(car L)) e) (cdr L)]
    [else (cons (car L) (encontFinish (cdr L) e))]))

;Funcion Selector
;Dominio: Lista X Entero X Entero
;Recorrido: "String"
;Descripcion: Regresa el jugador con la mayor puntuación
;Tipo: Cola
(define (reordenar L e a)
  (cond
    [(empty? L) a]
    [(>= (car(cdr(cdr(car L)))) e) (reordenar (cdr L) (car(cdr(cdr(car L)))) (car(car L)))]
    [else (reordenar (cdr L) e a)]))

;Funcion Modificador
;Dominio: Lista X Entero
;Recorrido: Lista
;Descripcion: Regresa a los jugadores por odern de puntaje 
;Tipo: Natural
(define (finish1 L i)            ;aclarar que el 1 significa primero y asi sucesivamente, es decir gana el 1
  (cond
    [(empty? L) null]
    [else (cons (list (reordenar L 0 0) i) (finish1 (encontFinish  L (reordenar L 0 0)) (+ i 1)))]))

;Funcion Modificador 
;Dominio: Lista 
;Recorrido: Lista
;Descripcion: regresa la tabla de posiciones de los jugadores
;Tipo: Declarativa
(define (finish L)             ;aclarar que el 1 significa primero y asi sucesivamente, es decir gana el 1
  (finish1 (car L) 1))
;Play---------------------------------------------------------------------------------------------------------------------
;Funcion Modificador
;Dominio: Lista X Funcion (o null)
;Recorrido: Lista (Game)
;Descripcion: funcion de jugar, realiza la accion segun la funcion establecida
;Tipo: Declarativa
(define play (lambda (L action)
               (cond
                 [(equal? action null) (nulo L)]
                 [(string? action)(Spotlt3 action L)]
                 [else (action L)])))

;----------------------------------------------------------------------------------------------------------------------
;----------------------------------TDA game - status-------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------
;Funcion Selector
;Dominio: Lista
;Recorrido: Lista
;Descripcion: Regresa el estado actual del juego
;Tipo: Declarativa
(define (status L)
  L)

;----------------------------------------------------------------------------------------------------------------------
;---------------------------------TDA game - score---------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------
;Funcion Selector
;Dominio: Lista X String
;Recorrido: Entero
;Descripcion: Regresa el puntaje de un jugador seleccionado
;Tipo: Cola
(define (score1 L name)
  (cond
    [(empty? L) null]
    [(equal? (car(car L)) name) (car(cdr(cdr(car L))))]
    [else (score1 (cdr L) name)]))

;Funcion Selector
;Dominio: Lista X String
;Recorrido: Entero
;Descripcion: Llama a la funcion anterior para encontrar el puntaje de cierto jugador
;Tipo: Declarativa
(define (score L name)
  (score1 (car L) name))
