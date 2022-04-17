#lang racket
(provide (all-defined-out))
;;;;;;;;;;;;;TDA Generacion de Cartas;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ListS(list "A " "B " "C " "D " "E " "F " "G " "H " "I " "J " "K " "L " "M " "N " "Ñ " "O " "P " "Q " "R " "S " "T " "U " "V " "W " "X " "Y " "Z "
                   "aA " "bB " "cC " "dD " "eE " "fF " "gG " "hH " "iI " "jJ " "kK " "lL " "mM " "nN " "ñÑ " "oO " "pP " "qQ " "rR " "sS " "tT " "uU " "vV " "wW " "xX " "yY " "zZ "
                   "aAa " "bBb " "cCc "))
(define ListSi(list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "Ñ" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
                   "aA" "bB" "cC" "dD" "eE" "fF" "gG" "hH" "iI" "jJ" "kK" "lL" "mM" "nN" "ñÑ" "oO" "pP" "qQ" "rR" "sS" "tT" "uU" "vV" "wW " "xX" "yY" "zZ"
                   "aAa" "bBb" "cCc"))
(define ListN(list "1 " "2 " "3 " "4 " "5 " "6 " "7 " "8 " "9 " "10 " "11 " "12 " "13 " "14 " "15 " "16 " "17 " "18 " "19 " "20 " "21 " "22 " "23 " "24 " "25 " "26 " "27 " "28 " "29 " "30 "
                   "31 " "32 " "33 " "34 " "35 " "36 " "37 " "38 " "39 " "40 " "41 " "42 " "43 " "44 " "45 " "46 " "47 " "48 " "49 " "50 " "51 " "52 " "53 " "54 " "55 " "56 " "57 "))

;Generacion de 1 Carta------------------------------------------------------------------------
;Funcion C1
;Dominio: entero X entero
;Recorrido: List
;Descripcion: recibe un numero y una constante = 1, y se usa para generar las n primeras cartas
;Tipo: Natural
(define (C1 n i)
  (cond
    [(<= i n) (cons i (C1 n (+ i 1)))]   ;agrega con recursion natural numeros de 1 hasta n
    [else null]))                        ;caso fin

;Generacion de N cartas-----------------------------------------------------------------------
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

;Generacion de las N*N Cartas-------------------------------------------------------------
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

;;;;;;;;;;;;; TDA cardsSet - constructor ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;       Simbologia        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;  random ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;Funcion cardsSet
;Dominio: Lista X Entero X Entero X Booleano
;Recorrido: Lista de Listas
;Descripcion: genera el conjunto de cartas completo con una lista de elementos, cantidad de elementos, numero max de cartas y si se quiere al azar o no
;Tipo: Declarativa
(define (cardsSet L N max FRandom)  ;random = #true / #false
  (cond
    [(equal? FRandom #true) (randomFn (cardsSet4 L N max) (random 1 (+ (length (cardsSet4 L N max))1)))]
    [else (cardsSet4 L N max)]))
  

;;;;;;;;;;;;; TDA cardsSet - numCards ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Funcion loopNc
;Dominio: Lista X Entero
;Recorrido: Entero
;Descripcion: Cuenta la cantidad de elementos en una lista (preferi generar mi popio length)
;Tipo: Cola
(define (loopNc L i)
  (cond
    [(empty? L) i]
    [else (loopNc (cdr L)(+ i 1))]))

;Funcion numCards
;Dominio: Lista
;Recorrido: Entero
;Descripcion: Regresa la cantidad de elementos en una lista
;Tipo: Declarativa
(define (numCards L)
  (loopNc L 0))

;;;;;;;;;;;;;;; TDA cardsSet - nthCard ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Funcion loopNth
;Dominio: Lista X Entero X Entero
;Recorrido: Elemento (o una Lista)
;Descripcion: regresa el elemento que se encuentre en sierta posición
;Tipo: Cola
(define (loopNth L j i)
  (cond
    [(empty? L) null]                      ;Caso lista vacia o fuera del alcance
    [(= (- j 1) i) (car L)]                ;Devuelve la carta solicitada
    [else (loopNth (rest L) j (+ i 1))]))  ;Cumple el loop sumando 1 a i

;Funcion nthCard
;Dominio: Lista X Entero 
;Recorrido: Lista
;Descripcion: regresa la carta que se encuentre en la posicion j
;Tipo: Declarativa
(define (nthCard L j)                      ;Funcion principal nthCard, L es lista; j el numero
  (loopNth L j 0))                         ;Envia los datos al loop

;;;;;;;;;;;;;;; TDA cardsSet - findTotalCards ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Funcion findTotalCards
;Dominio: Lista
;Recorrido: Entero
;Descripcion: A partir de una carta de muestra, determina la cantidad total de cartas que se deben producir para construir un conjunto válido.
;Tipo: Declarativa
(define (findTotalCards L)
  (+(*(- (length L )1)(- (length L)1))(length L)))  

;;;;;;;;;;;;;;; TDA cardsSet - Dobble? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Funcion buscar
;Dominio: Lista X Lista X Lista X Entero
;Recorrido: Entero
;Descripcion entrega el numero de elementos repetidos que hay entre ambas listas.
;Tipo: Cola
(define (buscar L1 L2 L12 acum) ;ej: (buscar (list 1 2 3 3 5 3 6) (list 1 2 3 4 5 6) (list 1 2 3 3 5 3 6) 0)
  (cond
    [(empty? L1) (buscar L12 (cdr L2) L12 acum)]
    [(empty? L2) acum]
    [(equal? (car L1) (car L2)) (buscar(cdr L1) L2 L12 (+ acum 1))]
    [else (buscar(cdr L1) L2 L12 acum)]))

;Funcion buscar3
;Dominio: Lista X Lista X Lista
;Recorrido: Booleano
;Descripcion recorre ambas listas y las envia a buscar. Solo 1 elemento en comun, de esta forma ve que el n-1 sea primo o un numero elevado a un primo.
;Tipo: Cola
(define (buscar3 L1 L2 L22)
  (cond
    [(empty? (cdr L1)) #true]
    [(empty? (cdr L2)) (buscar3 (cdr L1) (cdr L22) (cdr L22))]
    [(= (buscar (car L1) (car(cdr L2)) (car L1) 0) 1) (buscar3 L1 (cdr L2) L22)]
    [else #false]))


;Funcion buscar2
;Dominio: Lista X Lista
;Recorrido: Booleano
;Descripcion: no elentos repetido en una misma carta.
;Tipo: Cola
(define (buscar2 L1 L2)
  (cond
    [(empty? L1) #true]
    [(= (buscar (car L1) (car L2) (car L1) 0) (length (car L1))) (buscar2 (cdr L1) (cdr L2))]
    [else #false]))

;Funcion buscar3
;Dominio: Listas
;Recorrido: Booleano
;Descripcion: misma cantidad de elementos.
;Tipo:Cola

(define (buscar4 L)
  (cond
    [(empty? (cdr L)) #true]
    [(= (length (car L)) (length (car(cdr L)))) (buscar4 (cdr L))]
    [else #false]))

;Funcion Dobble?
;Dominio: Lista
;Recorrido: Booleano
;Descripcion: Recopilacion de anteriores y define si es un conjunto valido o no
;Tipo: Declarativa
(define (Dobble? L)
  (cond
    [(buscar4 L)(cond
                  [(buscar2 L L) (cond
                                   [(buscar3 L L L) #true]
                                   [else #false])]
                  [else #false])]
    [else #false]))


;;;;;;;;;; TDA cardsSet - requiredElements  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Funcion requiredElements
;Dominio: Lista
;Recorrido: Entero
;Descripcion: A partir de una carta de muestra, determina la cantidad total de elementos para un mazo 
;Tipo: Declarativa
(define (requiredElements L)
  (findTotalCards L))

;;;;;;;;;; TDA cardsSet - missingCards  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(RecoMiss (cardsSet ListN 3 -1 #t) (cardsSet ListN 3 4 #t) (cardsSet ListN 3 4 #t))
;Funcion RecoMiss
;Dominio: Lista X Lista X Lista
;Recorrido: Listas de listas
;Descripcion: regresa la cantidad de cartas para que el conjunto sea valido
;Tipo: Natural
(define (RecoMiss L1 L2 L22)
  (cond
    [(empty? L1) null]                                              ;cuando la lista se acaba 
    [(empty? L2) (cons (car L1) (RecoMiss (cdr L1) L22 L22))]       ;cuando el elemento se encontro se agrega a lista
    [(equal? (car L1)(car L2))(RecoMiss (cdr L1) L22 L22)]          ;cuando el elemento si esta en la lista se descarta y sigue
    [else (RecoMiss L1 (cdr L2) L22)]))                             ;caso de avance al siguiente elemento de la lista

;Funcion missingCards L
;Dominio: Lista (cartas)
;Recorrido: Lista de Listas
;Descripcion: a base de un conjunto de cartas, te entrega el resto de estas 
;Tipo: Declarativa
(define (missingCards L)
  (cond
    [(>= (buscar (car(cardsSet ListN (numCards (car L)) -1)) (car L) (car(cardsSet ListN (numCards (car L)) -1)) 0) 1) (RecoMiss(cardsSet ListN (numCards (car L)) -1) L L)]
    [else (RecoMiss(cardsSet ListS (numCards (car L)) -1) L L)]))

;;;;;;;;;; TDA cardsSet - CardsSet->String  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Funcion CardsSet->String2
;Dominio: Lista 
;Recorrido: Lista
;Descripcion: transforma cada carta en un string
;Tipo: Natural
(define (CardsSet->String2 L)
  (cond
    [(empty? L) null]
    [else (cons (apply string-append (car L))(CardsSet->String2 (cdr L)))]))

;Funcion putNCard
;Dominio: Lista X Entero
;Recorrido: Lista
;Descripcion: agrega elementos para el aspecto de cada carta
;Tipo: Natural
(define (putNCard L i)
  (cond
    [(empty? L) null]
    [else (cons(append (list "Card " (number->string  i) ": " (car L)) (list "\n")) (putNCard (cdr L) (+ i 1)))]))

;Funcion CardsSet->String
;Dominio: Lista
;Recorrido: String
;Descripcion: transforma la Lista de Strings genera anteriormente en un String
;Tipo: Declarativa
(define (CardsSet->String L)
  (apply string-append(CardsSet->String2 (putNCard (CardsSet->String2 L) 1))))


;;;;;;;;;; TDA cardsSet - addCard ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Funcion addCard2
;Dominio: Lista X Lista
;Recorrido: Lista de Listas
;Descripcion: agrega una carta a la lista
;Tipo: Natural
(define (addCard2 L c)
  (cond
    [(empty? L) (cons c null)]
    [else (cons (car L) (addCard2 (cdr L) c))]))

;Funcion addCard
;Dominio: Lista X Lista
;Recorrido: Lista de Listas
;Descripcion: Comprueba si el set con la carta nueva es valido, si lo es la agrega y si no lo es no la agrega
;Tipo: Declarativa
(define (addCard L c)
  (cond
    [(Dobble? (addCard2 L c)) (addCard2 L c)]
    [else L]))

