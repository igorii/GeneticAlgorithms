#lang racket

(require "util.rkt" 
         (prefix-in ga: "general-ga.rkt"))

(define (simple-max-fitness strlen)
  (let ([half-mark (/ strlen 2)])
    (λ (individual)
       (let* ([numer (foldl * 1 (take individual half-mark))]
              [denom (foldl * 1 (take (reverse individual) half-mark))])
           (/ numer denom)))))

(define (create-individual strlen)
  (map (λ (x) (+ 1 (random 10)))
       (range 0 strlen)))

(define (mutate-individual p individual)
  (define (m x) (+ 1 (random 10)))
  (map (λ (x) (if (chance p) (m x) x))
       individual))

(ga:run-ga #:string-length     10
           #:population-size   80
           #:selection-type    'tournament
           #:create-individual create-individual
           #:mutate-fn         mutate-individual
           #:cutoff            ((simple-max-fitness 10) (list 10 10 10 10 10 1 1 1 1 1))
           #:use-elitism       #t
           #:fitness-eval      (simple-max-fitness 10))

