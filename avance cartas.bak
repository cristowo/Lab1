#lang racket
(define Cards1 (lambda(n)
                 (if (= n 0) null
                     (cons n (Cards1(- n 1))))))


(define (C10 n i j)
  (cond
    [(< i n)(cons (+(+(+ n 1)(* n j))(+ i 1)) (C10 n (+ i 1) j))]
    [else null]))

(define (C11 n i j)
  (cond
    [(< j n) (cons (C10 n i j)(C11 n i (+ j 1)))]
    [else null]))


(define card(lambda (n k)
                   (cons(Cards1 n)(C11 (- n 1) 0 0))))