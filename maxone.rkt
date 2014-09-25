#lang racket

(define *string-length*   50)
(define *population-size* 10)
(define *print-all*       #t)

;; UTIL ;;
(define (zip l1 l2) (map cons l1 l2))
(define (random-under high) (floor (* (random) high)))
(define (print-with-cross-point chromosome cross-point)
  (define (f x curr acc)
    (if (null? x) 
      (begin (display acc) (newline))
      (if (= curr cross-point)
        (begin
          (let ([final (append acc (list #\_) x)])
            (display final) (newline)))
        (f (cdr x) (add1 curr) (append acc (list (car x)))))))
  (f chromosome 0 '()))
;; END UTIL

;; Add a type for the chromosome
;(define-type Chromosome (Listof Number))

;(: initialize-population (Number Number -> (Listof Chromosome)))
;; Initialize a random population of `popsize` many
;; individuals of length `strlen`
(define (initialize strlen popsize) 

  ;; Initialize one random individual
  (define (init-one strlen) 
    (map (λ (x) (if (< 0.5 (random)) 0 1)) 
         (range 0 strlen)))

  (map (λ (x) (init-one strlen)) ; Generate the entire population
       (range 0 popsize)))

;(: fitness (Chromosome -> Number))
;; Assess the fitness of an individual
(define (fitness individual)
  (foldl + 0 individual))

;(: mutate (Chromosome -> Chromosome))
;; Apply a random mutation to an individual
(define (mutate individual) 
  null)

;(: recombine (Chromosome Chromosome Number -> (Listof Chromosome)))
;; Apply recombinational crossover to two parents to
;; produce two children
(define (recombine parent1 parent2 cross-point)

  (define (get-child op cross-point p1 p2)
    (define (f p1 p2 curr acc)
      (if (or (null? p1) (null? p2))
        acc
        (let ([next (if (op curr cross-point) (car p1) (car p2))])
          (f (cdr p1) (cdr p2) (add1 curr) (append acc (list next))))))
    (f parent1 parent2 0 '()))

  (list (get-child <= cross-point parent1 parent2)
        (get-child >  cross-point parent1 parent2)))

;;(map fitness (initialize *string-length* *population-size*))
(define cp (random-under *string-length*))
(display "Crossover point ") (display cp) (newline)

(define parents (initialize *string-length* 2))
(display "Parent 1 ")
(print-with-cross-point (car parents) cp)
(display "Parent 2 ")
(print-with-cross-point (cadr parents) cp)

(define children (recombine (car parents) (cadr parents) cp))
(display "Child  1 ")
(print-with-cross-point (car children) cp)
(display "Child  2 ")
(print-with-cross-point (cadr children) cp)

