#lang racket
(require "GenerarCards_20814010-8_MarchantOsorio.rkt")

;TDA - Game-------------------------------------------------------------------------------------

;TDA game - constructor-------------------------------------------------------------------------
(define (player n)
  (cond
    [(= n 0) null]
    [else (cons (list n 0) (player (- n 1)))]))

(define game (lambda (Np L Modo FR)
               (cond
                 [(equal? Np 0) null]
                 [else (cons (player Np) (Modo (cdr L)))])))

;TDA game - stackMode----------------------------------------------------------------------------
(define (StackMode L)
  (list (list (encontrar L (length L)) (encontrar L (-(length L)1))) (eleminar (eleminar L (length L)) (-(length L)1))))

;TDA game - register-----------------------------------------------------------------------------
(define (insertarRegister L e)
  (cond
    [(empty? L) null]
    [(equal? (car(car L)) e) L]
    [(number? (car(car L))) (cons (list e 0) (cdr L))]
    [else (cons (car L) (insertarRegister (cdr L) e))]))
    
(define (register name L)
    (cons (insertarRegister (car L) name) (cdr L)))

;TDA game - whoseTurnIsIt?-----------------------------------------------------------------------
(define (turno? L L1)
  (cond
    [(empty? (cdr L)) (car(car L1))]
    [(equal? (car(cdr(car L))) (car(cdr(car(cdr L))))) (turno? (cdr L) L1)]
    [(> (car(cdr(car L)))  (car(cdr(car(cdr L))))) (car(car(cdr L)))]))
                 
(define (whoseTurnIsIt? L)
  (turno? (car L) (car L)))

;TDA game - play-----------------------------------------------------------------------------------













(define mazo (cardsSet ListN 5 -1 #f))
(define game1 (game 3 mazo StackMode 1))
