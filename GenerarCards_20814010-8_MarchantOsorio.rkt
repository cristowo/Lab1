#lang racket
;;;;;;;;;;;;;TDA Generacion de Cartas;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;; TDA cardsSet - constructor ;;;;;;;;;;;;;;;;;;;

;main principal para la función
(define Cards(lambda(n)
          (append(cons(C1 n 1)(C3 (- n 1) 1 1))(C6 (- n 1) 1 1 1))))

(define cardSet2 (lambda(L Num Max)   ;suponiendo que se basara meramente en numeros L = null
                 (cond
                   [(> Max 0)(cons(car L)(cardSet2 (cdr L) Num (- Max 1)))]
                   [(< Max 0) (Cards Num)]
                   [else null])))

(define cardsSet (lambda(L Num Max)
                  (define L1 (Cards Num))
                  (cardSet2 L1 Num Max)))

;;;;;;;;;;;;; TDA cardsSet - numCards ;;;;;;;;;;;;;;;;;;;
(define (loopNc L i)
  (cond
    [(empty? L) i]
    [else (loopNc (cdr L)(+ i 1))]))

(define (numCards L)
  (define i 0)
  (loopNc L i))

;;;;;;;;;;;;;;; TDA cardsSet - nthCard ;;;;;;;;;;;;;;;;;;;;    (nthCard (cardsSet null 8 -1) 1)
(define (loopNth L j i)
  (cond
    [(empty? L) null]                      ;Caso lista vacia o fuera del alcance
    [(= (- j 1) i) (car L)]                ;Devuelve la carta solicitada
    [else (loopNth (rest L) j (+ i 1))]))  ;Cumple el loop sumando 1 a i

(define (nthCard L j)                      ;Funcion principal nthCard, L es lista; j el numero
  (define i 0)                             ;Definimos un i auxiliar
  (loopNth L j i))                         ;Envia los datos al loop

;;;;;;;;;;;;;;; TDA cardsSet - findTotalCards ;;;;;;;;;;;;;;;;;;;;
(define (findTotalCards L)
  (define n (numCards L))
  (+(*(- n 1)(- n 1)) n))

;;;;;;;;;;;;;;; TDA cardsSet - Dobble? ;;;;;;;;;;;;;;;;;;;;
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
;Descripcion recorre ambas listas y las envia a buscar. Solo 1 elemento en comun 
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
    [(= (buscar (car L1) (car L2) (car L1) 0) (numCards (car L1))) (buscar2 (cdr L1) (cdr L2))]
    [else #false]))

;Funcion buscar3
;Dominio: Listas
;Recorrido: Booleano
;Descripcion: misma cantidad de elementos.
;Tipo:

(define (buscar4 L)
  (cond
    [(empty? (cdr L)) #true]
    [(= (numCards (car L)) (numCards (car(cdr L)))) (buscar4 (cdr L))]
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



























