#lang racket

(require "util.rkt")
(provide run-ga
         initialize-population 
         create-one-random-binary
         crossover-one-point
         print-with-cross-point)

;; (: print-with-cross-point ( Chromosome Number -> IO () ))
(define (print-with-cross-point chromosome cross-point)
  (define (loop x curr acc)
    (if (null? x) 
      (begin (display acc) (newline))
      (if (= curr cross-point)
        (begin
          (let ([final (append acc (list #\_) x)])
            (display final) (newline)))
        (loop (cdr x) (add1 curr) (append acc (list (car x)))))))
  (loop chromosome 0 '()))

;; (: create-one-random-binary ( Number -> Chromosome ))
;; Initialize one random individual
(define (create-one-random-binary strlen)
  (map (λ (x) (if (chance 0.5) 1 0))
       (range 0 strlen)))

;; (: initialize-population (Number Number -> (Listof Chromosome)))
;; Initialize a random population of `popsize` many
;; individuals of length `strlen`
(define (initialize-population create-fn strlen popsize)
  (define (loop n acc-dict)
    (if (= n 0)
      (map car (hash->list acc-dict))
      (let ([candidate (create-fn strlen)])
        (if (hash-has-key? acc-dict candidate)
          (loop n acc-dict) ; To ensure diversity in the initial population, skip any duplicates
          (loop (- n 1) (hash-set acc-dict candidate null))))))
  (loop popsize (make-immutable-hash '()))) ; Initialize all individuals

;; (: fitness (Chromosome -> Number))
(define (total-fitness fs) (foldl + 0 fs))

;; (: mutate (Real Chromosome -> Chromosome))
;; Apply a random mutation to an individual
(define (binary-mutate-fn p individual)
  (define (swap x) (if (= x 0) 1 0))
  (map (λ (x) (if (chance p) (swap x) x)) individual))

;; (: crossover-one-point ( Chromosome Chromosome Number -> (Listof Chromosome) ))
;; Apply recombinational crossover to two parents to
;; produce two children
(define (crossover-one-point parent1 parent2 cross-point)
  (define (get-child op cross-point p1 p2)
    (define (loop p1 p2 curr acc)
      (if (or (null? p1) (null? p2))
        acc
        (let ([next (if (op curr cross-point) (car p1) (car p2))])
          (loop (cdr p1) (cdr p2) (add1 curr) (append acc (list next))))))
    (loop parent1 parent2 0 '()))

  ;; Create the two children
  (list (get-child <= cross-point parent1 parent2)
        (get-child >  cross-point parent1 parent2)))

;; (: selection-roulette ( (Listof Chromosome) (Listof Number) -> (Listof Chromosome) ))
;; from slide 19
(define (selection-roulette pop fs)
  (define (loop wp partial-sum pop fs)
    (let ([next-fitness (+ partial-sum (car fs))])
      (cond [(null? (cdr fs))    (car pop)]
            [(> next-fitness wp) (car pop)]
            [else                (loop wp next-fitness (cdr pop) (cdr fs))])))
  (loop (* (random) (total-fitness fs)) 0 pop fs))

;; (: selection-tournament ( Number (Listof Chromosome) (Listof Number) -> (Listof Chromosome) ))
(define (selection-tournament size pop fs)
  (let* ([fs^pop       (zip fs pop)]
         [participants (take (shuffle fs^pop) size)]
         [winners      (take (sort participants (λ (f^p1 f^p2) (< (car f^p1) (car f^p2)))) 2)])
    winners))

(define (get-selection-fn type)
  (cond [(eq? type 'roulette)  selection-roulette]
        [(eq? type 'tournament) selection-tournament]))



;; Generic GA
(define (run-ga #:string-length     strlen
                #:population-size   popsize
                #:minimum-fitness   [minfit          0]
                #:cutoff            [cutoff          1]
                #:create-individual [create-fn       create-one-random-binary]
                #:use-elitism       [use-elitism     #f]
                #:mutate-fn         [mutate-fn       binary-mutate-fn]
                #:mutation-prob     [mutate-prob     (exact->inexact (/ 1 strlen))]
                #:selection-type    [selection-type  'roulette]
                #:fitness-eval      fitness-fn)


  (define (create-next-gen selection-type last-gen fits popsize strlen)
    (define (loop i acc)
      (if (= i 0)
        acc
        ;   Pick p1 with replacement
        (let* ([selection-fn (get-selection-fn selection-type)]
               [parent1     (selection-roulette last-gen fits)]
               ;   Pick p2 with replacement
               [parent2     (selection-roulette last-gen fits)]
               ;   Create children c1, c2 from parents
               [cross-point (random strlen)]
               [children    (crossover-one-point parent1 parent2 cross-point)]
               ;   Mutate both children
               [mutated-c1  (mutate-fn mutate-prob (car  children))]
               [mutated-c2  (mutate-fn mutate-prob (cadr children))])
          ;   Add mutated children to next population
          (loop (- i 1) (append acc (list mutated-c1 mutated-c2))))))

    ; Initialize the next population as an empty list
    ; For half the population size, do
    (loop (floor (/ popsize 2)) '()))


  (define (elitism best pop)
    (cons best (cdr (shuffle pop))))


  (define (master-loop pop)
    (let* ([fits        (map fitness-fn pop)]   ; Assess each individual
           [zipfits     (zip fits pop)]         ; Pair individuals with population
           [curr-best   (argmax car zipfits)])  ; Find the best in the current population
      (begin (display curr-best) (newline)
             ;   If best is good enough, return it
             (if (>= (car curr-best) cutoff)
               curr-best  ;; returns a pair of (fitness . individual)
               (if use-elitism
                 (master-loop (elitism (cdr curr-best) (create-next-gen selection-type pop fits popsize strlen)))
                 (master-loop (create-next-gen selection-type pop fits popsize strlen)))))))


  ; Initialize population of individuals
  (let ([init-pop (initialize-population create-fn strlen popsize)])
    (master-loop init-pop)))
