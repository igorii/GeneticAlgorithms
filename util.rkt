#lang racket

(provide chance zip)
(define (zip l1 l2) (map cons l1 l2))
(define (chance percent-true) (>= percent-true (random)))
