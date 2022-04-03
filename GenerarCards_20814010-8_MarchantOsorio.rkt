#lang racket
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
    [(<= i n)(cons(C5 n i j k)(C6 n (+ i 1) j k))]
    [else null]))

;main principal para la función
(define Cards(lambda(n)
          (cons(cons(C1 n 1)(C3 (- n 1) 1 1))(C6 (- n 1) 1 1 1))))




