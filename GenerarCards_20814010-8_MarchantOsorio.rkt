#lang racket
;;;;;;;;;;;;;TDA Generacion de Cartas;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ListS(list "A " "B " "C " "D " "E " "F " "G " "H " "I " "J " "K " "L " "M " "N " "Ñ " "O " "P " "Q " "R " "S " "T " "U " "V " "W " "X " "Y " "Z "
                   "aA " "bB " "cC " "dD " "eE " "fF " "gG " "hH " "iI " "jJ " "kK " "lL " "mM " "nN " "ñÑ " "oO " "pP " "qQ " "rR " "sS " "tT " "uU " "vV " "wW " "xX " "yY " "zZ "
                   "aAa " "bBb " "cCc "))
(define ListN(list "1 " "2 " "3 " "4 " "5 " "6 " "7 " "8 " "9 " "10 " "11 " "12 " "13 " "14 " "15 " "16 " "17 " "18 " "19 " "20 " "21 " "22 " "23 " "24 " "25 " "26 " "27 " "28 " "29 " "30 "
                   "31 " "32 " "33 " "34 " "35 " "36 " "37 " "38 " "39 " "40 " "41 " "42 " "43 " "44 " "45 " "46 " "47 " "48 " "49 " "50 " "51 " "52 " "53 " "54 " "55 " "56 " "57 "))
;se usa para generar las n primeras cartas
(define (C1 n i)
  (cond
    [(<= i n) (cons i (C1 n (+ i 1)))]
    [else null]))
;se usa para generar por filas de tamaño n,numeros consecutivos 
(define (C2 n j k)
  (cond
    [(<= k n) (cons (+(* n j)(+ k 1))(C2 n j (+ k 1)))]
    [else null]))

(define (C3 n j k)
  (cond
    [(<= j n)(cons (cons 1 (C2 n j k))(C3 n (+ j 1) k))]
    [else null]))
;se usa para generar numeros consecutivos por columnas de tamaño n
(define (C4 n i j k)
  (cond
    [(<= k n)(cons (+(+ (+ (modulo(-(+(*(- i 1)(- k 1))j)1)n) (* n(- k 1))) 2) n) (C4 n i j (+ k 1)))]
    [else null]))
(define (C5 n i j k)
  (cond
    [(<= j n)(cons(cons (+ i 1)(C4 n i j k))(C5 n i (+ j 1) k))]
    [else null]))
(define (C6 n i j k)
  (cond
    [(<= i n)(append(C5 n i j k)(C6 n (+ i 1) j k))]
    [else null]))

;;;;;;;;;;;;; TDA cardsSet - constructor ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;main principal para la función
(define Cards(lambda(n)
          (append(cons(C1 n 1)(C3 (- n 1) 1 1))(C6 (- n 1) 1 1 1))))

(define cardSet2 (lambda(L Num Max)   ;suponiendo que se basara meramente en numeros L = null
                 (cond
                   [(> Max 0)(cons(car L)(cardSet2 (cdr L) Num (- Max 1)))]
                   [(< Max 0) (Cards Num)]
                   [else null])))

;;;;;;;;;;;;;;;;       Simbologia        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (recorrer Ls n acum)
  (cond
    [(empty? Ls) n] ;se puede poner null para que no exista el numero o se puede poner n para que este el numero.
    [( = acum (- n 1)) (car Ls)]
    [else (recorrer (cdr Ls) n (+ acum 1))]))
       
(define (igualar Ls Lc)
  (cond
    [(empty? Lc) null]
    [else (cons (recorrer Ls (car Lc) 0) (igualar Ls (cdr Lc)))]))

(define (Symbo Ls Lc)
  (cond
    [(empty? Lc) null]
    [else (cons(igualar Ls (car Lc))(Symbo Ls (cdr Lc)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cardsSet3 (lambda(L Num Max)
                  (define L1 (Cards Num))
                  (cardSet2 L1 Num Max)))

(define cardsSet (lambda(L Num Max)
                   (cond
                     [(empty? L) (cardsSet3 L Num Max)]                ;(cardsSet null 3 -1)
                     [else (Symbo L (cardsSet3 L Num Max))])))         ;(cardsSet (list "a" "b" "c" "d" "e" "f" "g") 3 -1)
;;;;;;;;;;;;; TDA cardsSet - numCards ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;preferi generar mi popio length
(define (loopNc L i)
  (cond
    [(empty? L) i]
    [else (loopNc (cdr L)(+ i 1))]))

(define (numCards L)
  (loopNc L 0))

;;;;;;;;;;;;;;; TDA cardsSet - nthCard ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (loopNth L j i)
  (cond
    [(empty? L) null]                      ;Caso lista vacia o fuera del alcance
    [(= (- j 1) i) (car L)]                ;Devuelve la carta solicitada
    [else (loopNth (rest L) j (+ i 1))]))  ;Cumple el loop sumando 1 a i

(define (nthCard L j)                      ;Funcion principal nthCard, L es lista; j el numero
  (loopNth L j 0))                         ;Envia los datos al loop

;;;;;;;;;;;;;;; TDA cardsSet - findTotalCards ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (findTotalCards L)
  (+(*(- (length L )1)(- (length L)1))(length L)))  

;;;;;;;;;;;;;;; TDA cardsSet - Dobble? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Funcion buscar
;Dominio: Listas y un entero
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
;Dominio: Listas
;Recorrido: Booleano
;Descripcion recorre ambas listas y las envia a buscar. Solo 1 elemento en comun, de esta forma ve que el n-1 sea primo o un numero elevado a un primo.
;Tipo: 
(define (buscar3 L1 L2 L22)
  (cond
    [(empty? (cdr L1)) #true]
    [(empty? (cdr L2)) (buscar3 (cdr L1) (cdr L22) (cdr L22))]
    [(= (buscar (car L1) (car(cdr L2)) (car L1) 0) 1) (buscar3 L1 (cdr L2) L22)]
    [else #false]))


;Funcion buscar2
;Dominio: Listas
;Recorrido: Booleano
;Descripcion: no elentos repetido en una misma carta.
;Tipo: 
(define (buscar2 L1 L2)
  (cond
    [(empty? L1) #true]
    [(= (buscar (car L1) (car L2) (car L1) 0) (length (car L1))) (buscar2 (cdr L1) (cdr L2))]
    [else #false]))

;Funcion buscar3
;Dominio: Listas
;Recorrido: Booleano
;Descripcion: misma cantidad de elementos.
;Tipo:

(define (buscar4 L)
  (cond
    [(empty? (cdr L)) #true]
    [(= (length (car L)) (length (car(cdr L)))) (buscar4 (cdr L))]
    [else #false]))

;Funcion Dobble?
;Dominio: Listas
;Recorrido: Booleano
;Descripcion: 
;Tipo:
(define (Dobble? L)
  (cond
    [(buscar4 L)(cond
                  [(buscar2 L L) (cond
                                   [(buscar3 L L L) #true]
                                   [else #false])]
                  [else #false])]
    [else #false]))


;;;;;;;;;; TDA cardsSet - requiredElements  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (requiredElements L)
  (findTotalCards L))


;;;;;;;;;; TDA cardsSet - missingCards  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(numCards L)
(define (RecoMiss L1 L2 L22)
  (cond
    [(empty? L1) null]                                              ;cuando la lista se acaba 
    [(empty? L2) (cons (car L1) (RecoMiss (cdr L1) L22 L22))]       ;cuando el elemento se encontro se agrega a lista
    [(equal? (car L1)(car L2))(RecoMiss (cdr L1) L22 L22)]          ;cuando el elemento si esta en la lista se descarta y sigue
    [else (RecoMiss L1 (cdr L2) L22)]))                             ;caso de avance al siguiente elemento de la lista

(define (missingCards L)
  (cond
    [(>= (buscar (car(cardsSet ListN (numCards (car L)) -1)) (car L) (car(cardsSet ListN (numCards (car L)) -1)) 0) 1) (RecoMiss(cardsSet ListN (numCards (car L)) -1) L L)]
    [else (RecoMiss(cardsSet ListS (numCards (car L)) -1) L L)]))

;;;;;;;;;; TDA cardsSet - CardsSet->String  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (CardsSet->String2 L)
  (cond
    [(empty? L) null]
    [else (cons (apply string-append (car L))(CardsSet->String2 (cdr L)))]))

(define (putNCard L)
  (cond
    [(empty? L) null]
    [else (cons(list "/Card: " (car L)) (putNCard (cdr L)))]))

(define (CardsSet->String L)
  (apply string-append(CardsSet->String2 (putNCard (CardsSet->String2 L)))))










