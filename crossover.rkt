#lang racket

(require "general-ga.rkt")

(let* ([strlen      50]
       [cross-point (random strlen)]
       [parents     (initialize-population create-one-random-binary strlen 2)]
       [children    (crossover-one-point (car parents) (cadr parents) cross-point)])
  (begin
    (display "Crossover point: ") (display cross-point) (newline)
    (display "Parent 1:        ") (print-with-cross-point (car parents) cross-point)
    (display "Parent 2:        ") (print-with-cross-point (cadr parents) cross-point)
    (display "Child  1:        ") (print-with-cross-point (car children) cross-point)
    (display "Child  2:        ") (print-with-cross-point (cadr children) cross-point)))
