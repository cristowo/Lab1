#lang racket
;--------Para Exportar Funciones---------------------------------------------------------------------------------------
(provide (all-defined-out))
;--------Listas Propuestas a usar (max cantidad de elementos 57)-------------------------------------------------------
(define ListS(list "A " "B " "C " "D " "E " "F " "G " "H " "I " "J " "K " "L " "M " "N " "Ñ " "O " "P " "Q " "R " "S " "T " "U " "V " "W " "X " "Y " "Z "
                   "aA " "bB " "cC " "dD " "eE " "fF " "gG " "hH " "iI " "jJ " "kK " "lL " "mM " "nN " "ñÑ " "oO " "pP " "qQ " "rR " "sS " "tT " "uU " "vV " "wW " "xX " "yY " "zZ "
                   "aAa " "bBb " "cCc "))
(define ListN(list "1 " "2 " "3 " "4 " "5 " "6 " "7 " "8 " "9 " "10 " "11 " "12 " "13 " "14 " "15 " "16 " "17 " "18 " "19 " "20 " "21 " "22 " "23 " "24 " "25 " "26 " "27 " "28 " "29 " "30 "
                   "31 " "32 " "33 " "34 " "35 " "36 " "37 " "38 " "39 " "40 " "41 " "42 " "43 " "44 " "45 " "46 " "47 " "48 " "49 " "50 " "51 " "52 " "53 " "54 " "55 " "56 " "57 "))
;-------------------------------------------REPRESENTACION--------------------------------------------------------------------------------------------
;TDA correspondiente a la creacion de un set de cartas

;----------------------------------------Generacion de 1 Carta------------------------------------------------------------------------
;Funcion C1
;Dominio: entero X entero
;Recorrido: List
;Descripcion: recibe un numero y una constante = 1, y se usa para generar las n primeras cartas
;Tipo: Natural
(define (C1 n i)
  (cond
    [(<= i n) (cons i (C1 n (+ i 1)))]   ;agrega con recursion natural numeros de 1 hasta n
    [else null]))                        ;caso fin

;----------------------------------------Generacion de N cartas-----------------------------------------------------------------------
;Funcion C2
;Dominio: Entero X Entero X Entero
;Recorrido: Lista
;Descripcion: recibe un numero y 2 constates = 1, y genera una carta sin el primer elemento
;Tipo: Cola
(define (C2 n j k)
  (cond
    [(<= k n) (cons (+(* n j)(+ k 1))(C2 n j (+ k 1)))]   ;genera un listado con recursion
    [else null]))

;Funcion C3
;Dominio:  Entero X Entero X Entero
;Recorrido: Lista de Listas
;Descripcion: recibe un numero n y 2 constantes = 1, y genera n cartas.
;Tipo: Cola
(define (C3 n j k)
  (cond
    [(<= j n)(cons (cons 1 (C2 n j k))(C3 n (+ j 1) k))]          ;agrega las cartas anteriores a una nueva lista
    [else null]))

;---------------------------------------Generacion de las N*N Cartas-------------------------------------------------------------
;Funcion C4
;Dominio: Entero X Entero X Entero X Entero
;Recorrido: Lista
;Descripcion: recibe un numero y 3 constates = 1, y genera una carta sin el primer elemento
;Tipo: Cola
(define (C4 n i j k)
  (cond
    [(<= k n)(cons (+(+ (+ (modulo(-(+(*(- i 1)(- k 1))j)1)n) (* n(- k 1))) 2) n) (C4 n i j (+ k 1)))]
    [else null]))

;Funcion C5
;Dominio: Entero X Entero X Entero X Entero
;Recorrido: Lista de listas
;Descripcion: recibe un numero y 3 constates = 1, y genera una parte del set de cartas de tamaño n
;Tipo: Cola
(define (C5 n i j k)
  (cond
    [(<= j n)(cons(cons (+ i 1)(C4 n i j k))(C5 n i (+ j 1) k))]
    [else null]))

;Funcion C6
;Dominio: Entero X Entero X Entero X Entero
;Recorrido: Lista de Listas
;Descripcion: recibe un numero y 3 constates = 1, y genera un mazo de cartas completo
;Tipo: Cola
(define (C6 n i j k)
  (cond
    [(<= i n)(append(C5 n i j k)(C6 n (+ i 1) j k))]
    [else null]))

;--------------------------------Creacion Mazo con Maximo de cartas----------------------------------------------------
;Funcion Cards
;Dominio: Entero
;Recorrido: Lista de Listas
;Descripcion: a partir de un N que representa la cantidad de elementos por cartas, genera un mazo completo
;Tipo: Cola
;main principal para la función
(define Cards(lambda(n)
          (append(cons(C1 n 1)(C3 (- n 1) 1 1))(C6 (- n 1) 1 1 1))))

;Funcion cardSet2 (recortar segun max)
;Dominio: Lista X Entero X Entero
;Recorrido: Lista de Listas (o si Max = 1, Lista)
;Descripcion: recorta la lista en en la posicion que se desee
;Tipo: Cola
(define cardSet2 (lambda(L Num Max)   ;suponiendo que se basara meramente en numeros L = null
                 (cond
                   [(> Max 0)(cons(car L)(cardSet2 (cdr L) Num (- Max 1)))]
                   [(< Max 0) (Cards Num)]
                   [else null])))

;-----------------------------Creacion de Mazo con Simbolos---------------------------------------------------------------------------
;Funcion recorrer
;Dominio: Lista X Entero X Entero
;Recorrido: String
;Descripcion: recorre la lista de simbolos, y retorna el elemento de la posicion indicada
;Tipo: Cola 
(define (recorrer Ls n acum)
  (cond
    [(empty? Ls) n] ;se puede poner null para que no exista el numero o se puede poner n para que este el numero.
    [( = acum (- n 1)) (car Ls)]
    [else (recorrer (cdr Ls) n (+ acum 1))]))

;Funcion igualar
;Dominio: Lista X Lista
;Recorrido: Lista
;Descripcion: a base de una carta, crea una carta de la lista de simbolos
;Tipo: Cola        
(define (igualar Ls Lc)
  (cond
    [(empty? Lc) null]
    [else (cons (recorrer Ls (car Lc) 0) (igualar Ls (cdr Lc)))])) ;agrega simbolo x simbolos hasta llenar la carta

;Funcion Symbo
;Dominio: Lista X Lista
;Recorrido: Lista de Listas
;Descripcion: genera una lista de cartas con simbolos
;Tipo: Cola
(define (Symbo Ls Lc)
  (cond
    [(empty? Lc) null]
    [else (cons(igualar Ls (car Lc))(Symbo Ls (cdr Lc)))]))  ;agrega carta por carta hasta llenar el mazo

;...................Mazos con la inclusion de listas................................................................................
;Funcion cardsSet3
;Dominio: Lista X Entero X Entero
;Recorrido: Lista de Listas 
;Descripcion: bajo los parametros de entrada genera un set de cartas
;Tipo: Declarativa 
(define cardsSet3 (lambda(L Num Max)
                  (define L1 (Cards Num))
                  (cardSet2 L1 Num Max)))

;Funcion cardsSet4 
;Dominio: Lista X Entero X Entero
;Recorrido: Lista de Listas 
;Descripcion: remplaza la lista de números por la lista de string ingresada
;Tipo: Declarativa
(define cardsSet4 (lambda(L Num Max)
                   (cond
                     [(empty? L) (cardsSet3 L Num Max)]                ;(cardsSet null 3 -1)
                     [else (Symbo L (cardsSet3 L Num Max))])))         ;(cardsSet (list "a" "b" "c" "d" "e" "f" "g") 3 -1)

;---------------------------------------RANDOM--------------------------------------------------------------------------------
;Funcion encotrar 
;Dominio: Lista X Entero
;Recorrido: 
;Descripcion: recorre la lista hasta encontrar el elemento por su posicion y lo devuelve
;Tipo: Cola
(define (encontrar L n)
  (cond
    [(empty? L) null]
    [(equal? n 1)(car L)]
    [else (encontrar (cdr L) (- n 1))]))

;Funcion eleminar
;Dominio: Lista X Entero
;Recorrido: Lista
;Descripcion: Recorre la lista hasta encontrar un elemento por su posicion y lo borra
;Tipo: Natural
(define (eleminar L n)
  (cond
    [(empty? L) null]
    [(equal? n 1)(cdr L)]
    [else (cons (car L) (eleminar (cdr L) (- n 1)))]))

;Funcion randomFn
;Dominio: Lista X Entero
;Recorrido: Lista 
;Descripcion: desordena elementos de una lista
;Tipo: Natural
(define (randomFn L NR)
  (cond
    [(equal? (length L) 1) (cons (car L) null)]
    [else (cons (encontrar L NR) (randomFn (eleminar L NR) (random 1 (+(length (eleminar L NR))1))))]))

;---------------------------------------TDA - cardsSet -------------------------------------------------------------------------------------------------------
;Funcion cardsSet
;Dominio: Lista X Entero X Entero X Booleano
;Recorrido: Lista de Listas
;Descripcion: genera el conjunto de cartas completo con una lista de elementos, cantidad de elementos, numero max de cartas y si se quiere al azar o no
;Tipo: Declarativa
(define (cardsSet L N max FRandom)  ;random = #true / #false
  (cond
    [(equal? FRandom #true) (randomFn (cardsSet4 L N max) (random 1 (+ (length (cardsSet4 L N max))1)))]
    [else (cardsSet4 L N max)]))

;---------------------------------------EJEMPLOS----------------------------------------------------------------------------
(define CARTA1 (cardsSet ListS 4 -1 #false)) ;mazo de 4 elementos 
(define CARTA2 (cardsSet ListS 4 3 #false))  ;mazo de 4 elementos y 3 cartas
(define CARTA3 (cardsSet ListS 4 -1 #true))  ;mazo de 4 elementos ordenados al azar
(define CARTA4 (cardsSet ListS 4 3 #true))   ;mazo de 4 elementos y 3 cartas ordenadas al azar