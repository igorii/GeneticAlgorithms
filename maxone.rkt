#lang racket

;; Data structures
;; PopulationTable
;;    individual -> fitness
;;    (hash-keys h) ; gets the keys as a list
;;    (hash-values h)
;;    (hash->list h) ; key value pairs

;; UTIL ;;
(define (zip l1 l2) (map cons l1 l2))
(define (random-int high) (round (* (random) high)))
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
(define (chance percent-true) (>= percent-true (random)))

;(: initialize-population (Number Number -> (Listof Chromosome)))
;; Initialize a random population of `popsize` many
;; individuals of length `strlen`
(define (initialize strlen popsize)

  ; Initialize one random individual
  (define (init-one strlen)
    (map (λ (x) (if (chance 0.5) 1 0))
         (range 0 strlen)))

  ; Initialize all individuals
  (define (f n acc-dict)
    (if (= n 0)
      (map car (hash->list acc-dict))
      (let ([candidate (init-one strlen)])
        (if (hash-has-key? acc-dict candidate)
          ; To ensure diversity in the initial population, skip any duplicates
          (f n acc-dict)
          (f (- n 1) (hash-set acc-dict (init-one strlen) null))))))
    (f popsize (make-immutable-hash '())))


  ;(map (λ (x) (init-one strlen)) ; Generate the entire population
  ;     (range 0 popsize)))

;(: fitness (Chromosome -> Number))
;; Assess the fitness of an individual
(define (fitness individual) (foldl + 0 individual))
(define (total-fitness fs)   (foldl + 0 fs))

;(: assess-all ((Listof Chromosome) -> (Listof Number)))
;; Assess the fitness of every individual
;(define assess-all map)

;(: mutate (Real Chromosome -> Chromosome))
;; Apply a random mutation to an individual
(define (mutate p individual) 
  (define (swap x) (if (= x 0) 1 0))
  (map (λ (x) (if (>= p (random)) (swap x) x)) individual))

;(: recombine (Chromosome Chromosome Number -> (Listof Chromosome)))
;; Apply recombinational crossover to two parents to
;; produce two children
(define (crossover-one-point parent1 parent2 cross-point)
  (define (get-child op cross-point p1 p2)
    (define (f p1 p2 curr acc)
      (if (or (null? p1) (null? p2))
        acc
        (let ([next (if (op curr cross-point) (car p1) (car p2))])
          (f (cdr p1) (cdr p2) (add1 curr) (append acc (list next))))))
    (f parent1 parent2 0 '()))

  ;; Create the two children
  (list (get-child <= cross-point parent1 parent2)
        (get-child >  cross-point parent1 parent2)))

;; from slide 19
(define (selection-roulette pop fs)
  (define (f wp partial-sum pop fs)
    (let ([next-fitness (+ partial-sum (car fs))])
      (cond [(null? (cdr fs))    (car pop)]
            [(> next-fitness wp) (car pop)]
            [else                (f wp next-fitness (cdr pop) (cdr fs))])))
  (f (* (random) (total-fitness fs)) 0 pop fs))

(define (selection-tournament size pop fs) 
  (let* ([fs.pop       (zip fs pop)]
         [participants (take (shuffle fs.pop) size)]
         [winners      (take (sort participants (λ (f.p1 f.p2) (< (car f.p1) (car f.p2)))) 2)])
    winners))



; Generic GA
; ==========

(define (GA #:string-length   strlen
            #:population-size popsize
            #:minimum-fitness [minfit 0]
            #:cutoff          cutoff
            #:mutation-prob   [mutate-prob (exact->inexact (/ 1 strlen))]
            #:fitness-eval    fitness-fn)

  (define (create-next-gen last-gen fits popsize strlen)
    (define (f i acc)
      (if (= i 0)
        acc
        ;   Pick p1 with replacement
        (let* ([parent1     (selection-roulette last-gen fits)]
               ;   Pick p2 with replacement
               [parent2     (selection-roulette last-gen fits)]
               ;   Create children c1, c2 from parents
               [cross-point (random-int strlen)]
               [children    (crossover-one-point parent1 parent2 cross-point)]
               ;   Mutate both children
               [mutated-c1  (mutate mutate-prob (car  children))]
               [mutated-c2  (mutate mutate-prob (cadr children))])
          ;   Add mutated children to next population
          (f (- i 1) (append acc (list mutated-c1 mutated-c2))))))

    ; Initialize the next population as an empty list
    ; For half the population size, do
    (f (floor (/ popsize 2)) '()))

  ; repeat
  (define (loop pop last-best)
    ; Assess each individual
    (let* ([fits      (map fitness-fn pop)]
           [zipfits   (zip fits pop)]
           ;   Update best
           [curr-best (argmax car zipfits)])

      (begin (display curr-best) (newline)

      ;   If best is good enough, return it
      (if (>= (car curr-best) cutoff)
        curr-best  ;; returns a pair of (fitness . individual)
        (loop (elitism (cdr last-best ) (create-next-gen pop fits popsize strlen)) curr-best)))))

  ; Initialize population of individuals
  (let ([init-pop (initialize strlen popsize)]
        ; Initialize best to min value
        [init-best (cons minfit null)])
    (loop init-pop init-best)))

(define (elitism best pop)
  (cons best (cdr (shuffle pop))))


;; TEST
;(initialize 50 10)




;(define (GA strlen popsize minfit cutoff fitness-fn)
(GA #:string-length   50
    #:population-size 100
    #:cutoff          50
    #:minimum-fitness 0
    #:fitness-eval    fitness)
;;
;(define population (initialize *string-length* *population-size*))
;(define fs (assess-all fitness population))
;(display population) (newline)
;(define parent1 (selection-roulette population fs))
;(define parent2 (selection-roulette population fs))
;;
;fs
;(fitness parent1)
;(fitness parent2)

;;(map fitness (initialize *string-length* *population-size*))
;(define cp (random-under *string-length*))
;(display "Crossover point ") (display cp) (newline)
;
;(define parents (initialize *string-length* 2))
;(display "Parent 1 ")
;(print-with-cross-point (car parents) cp)
;(display "Parent 2 ")
;(print-with-cross-point (cadr parents) cp)
;
;(define children (recombine (car parents) (cadr parents) cp))
;(display "Child  1 ")
;(print-with-cross-point (car children) cp)
;(display "Child  2 ")
;(print-with-cross-point (cadr children) cp)
;(display "Mutation ")
;(print-with-cross-point (mutate 0.1 (cadr children)) cp)
;
;(assess-all fitness (append parents children))
