#lang racket

(require (prefix-in ga: "general-ga.rkt"))

;; (: fitness (Chromosome -> Number))
;; Assess the fitness of an individual
(define (max-one-fitness individual)
  (foldl + 0 individual))

(ga:run-ga #:string-length   50
        #:population-size 100
        #:cutoff          50
        #:use-elitism     #t
        #:fitness-eval    max-one-fitness)
