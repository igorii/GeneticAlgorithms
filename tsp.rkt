#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "util.rkt"
         "general-ga.rkt"
         "window.rkt")

;; Utils
(define (fpop->pop  fpop) (map cdr fpop))
(define (fpop->fits fpop) (map car fpop))
(define (line-distance p1 p2)
  (sqrt (sqr (+ (- (car  p1) (car  p2))
                (- (cadr p1) (cadr p2))))))
;; End utils

;; Crossover helperfn
(define (phase1 p1 p2 a b new-tour used-map)
  (define (loop pos new-tour p1 p2 used-map)
    (cond [(null? p1) (list new-tour used-map)]
          [(null? p2) (list new-tour used-map)]
          [(> pos b)  (loop (add1 pos) (append new-tour (list null)) (cdr p1) (cdr p2) used-map)]
          [(< pos a)  (loop (add1 pos) (append new-tour (list null)) (cdr p1) (cdr p2) used-map)]
          [else (loop (add1 pos)
                      (append new-tour (list (car p1)))
                      (cdr p1)
                      (cdr p2)
                      (hash-set used-map (car p1) #t))]))
  (loop 0 new-tour p1 p2 used-map))

;; Crossover helperfn
(define (phase2 p1 p2 a b new-tour used-map)
  (define (loop pos src-new-tour dest-new-tour p1 p2 used-map)
    (cond [(null? p1) (list dest-new-tour used-map)]
          [(null? p2) (list dest-new-tour used-map)]
          [(and (<= pos b) (>= pos a)) (loop (add1 pos)
                                             (cdr src-new-tour)
                                             (append dest-new-tour (list (car src-new-tour))) 
                                             (cdr p1)
                                             (cdr p2)
                                             used-map)]
          [(hash-has-key? used-map (car p2)) (loop (add1 pos)
                                                   (cdr src-new-tour)
                                                   (append dest-new-tour (list (car src-new-tour))) 
                                                   (cdr p1)
                                                   (cdr p2)
                                                   used-map)]
          [else (begin (display (car p2)) (newline)
                       (loop (add1 pos)
                             (cdr src-new-tour)
                             (append dest-new-tour (list (car p2)))
                             (cdr p1)
                             (cdr p2)
                             (hash-set used-map (car p2) #t)))]))
  (begin (display "phase2")(newline)
         (loop 0 new-tour '() p1 p2 used-map)))

(define (get-next-unused p u)
  (cond [(null? p) p]
        [(not (hash-has-key? (car p))) p]
        [else (get-next-unused (cdr p) u)]))

;; This is wrong, needs to be mutual recursion
;; between p1 and src-new-tour looking for null places
(define (phase3 p1 p2 new-tour used-map)
  (define (loop p1 p2 src-new-tour dest-new-tour)
    (cond [(null? p1) dest-new-tour]
          [(null? p2) dest-new-tour]
          [(hash-has-key? used-map (car p2)) (loop (cdr p1)
                                                   (cdr p2)
                                                   (cdr src-new-tour)
                                                   (append dest-new-tour (list (car src-new-tour))))]
          [else (loop (cdr p1)
                      (cdr p2)
                      (cdr src-new-tour)
                      (append dest-new-tour (list (car p2))))]))
  (loop p1 p2 new-tour '()))

(define (crossover-partially-mapped p1 p2 strlen) 
  (let* ([r1           (random strlen)]       ; Create two random points for crossover
         [r2           (random strlen)]
         [point-a      (if (> r1 r2) r2 r1)]  ; Get the min point for corssover
         [point-b      (if (> r1 r2) r1 r2)]  ; Get the max point for crossover
         [used-map     #hash()]               ; Store used cities here to avoid linear lookup
         [new-tour     '()]
         [post-phase1  (phase1 p1 p2 point-a point-b new-tour used-map)]
         [post-phase2  (phase2 p1 p2 point-a point-b (car post-phase1) (cadr post-phase1))]
         [post-phase3  (phase3 p1 p2 (car post-phase2) (cadr post-phase2))])
    (begin
      (display "Parent 1: ") (display p1) (newline)
      (display "Parent 2: ") (display p2) (newline)
      (display "Points  : ") (display (list point-a point-b)) (newline)
      post-phase3)))

;; two of
;(define (crossover-partially-mapped p1 p2 strlen) 
;
;  (define (phase1 a b new-tour1 new-tour2 used1-map used2-map)
;    (define (loop pos new-tour1 new-tour2 p1 p2 used1-map used2-map)
;      (cond [(null? p1) (list new-tour1 new-tour2 used1-map used2-map)]
;            [(null? p2) (list new-tour1 new-tour2 used1-map used2-map)]
;            [(> pos b)  (list new-tour1 new-tour2 used1-map used2-map)]
;            [(< pos a)  (loop (add1 pos) 
;                              (cons null new-tour1) 
;                              (cons null new-tour2) 
;                              (cdr p1) 
;                              (cdr p2)
;                              used1-map
;                              used2-map)]
;            [else (loop (add1 pos)
;                        (append new-tour1 (list (car p1)))
;                        (append new-tour2 (list (car p2)))
;                        (cdr p1)
;                        (cdr p2)
;                        (hash-set used1-map (car p1) #t)
;                        (hash-set used2-map (car p2) #t))]))
;    (loop 0 new-tour1 new-tour2 p1 p2 used1-map used2-map))
;
;  (define (phase2 a b new-tour1 new-tour2 used1-map used2-map)
;    (define (loop pos src-new-tour1 dest-new-tour1 src-new-tour2 dest-new-tour2 p1 p2 used1-map used2-map)
;      (cond [(null? p1) (list new-tour1 new-tour2 used1-map used2-map)]
;            [(null? p2) (list new-tour1 new-tour2 used1-map used2-map)]
;            [(and (<= pos b) (>= pos a)) (loop (add1 pos) 
;                                               (cdr src-new-tour1)
;                                               (append dest-new-tour1 (list (car src-new-tour1)))
;                                               (cdr src-new-tour2)
;                                               (append dest-new-tour2 (list (car src-new-tour2)))
;                                               (cdr p1)
;                                               (cdr p2)
;                                               used1-map
;                                               used2-map)]
;      )
;    ))
;
;  (let* ([r1           (random strlen)]       ; Create two random points for crossover
;         [r2           (random strlen)]
;         [point-a      (if (> r1 r2) r2 r1)]  ; Get the min point for corssover
;         [point-b      (if (> r1 r2) r1 r2)]  ; Get the max point for crossover
;         [used1-map     #hash()]       ; Store used cities here to avoid linear lookup
;         [used2-map     #hash()]       ; Store used cities here to avoid linear lookup
;         [new-tour1    '()]
;         [new-tour2    '()])
;
;    (begin (display (list point-a point-b)) (newline)
;
;    (phase1 point-a point-b new-tour1 new-tour2 used1-map used2-map))))

;(define r (crossover-partially-mapped '(1 2 3 4 5 6 7 8 9 10) '(10 9 8 7 6 5 4 3 2 1) 10))
;(display "C1  : ") (display r) (newline)

(define (crossover-injection p1 p2)      null)
(define (crossover-order p1 p2)          null)
(define (crossover-position-based p1 p2) null)

;; two mutation operators
(define (mutation-1 individual) null)
(define (mutation-2 individual) null)

;; Two of
(define (selection-ranked pop)   null)
(define (selection-roulette pop) null)

;; selection-tournament : ( Number -> Real -> (Listof '( Number '( Number Number ))) )
(define (selection-tournament tsize bprob fpop)
  (let* ([tpop (take (shuffle fpop) tsize)])       ; Select the individuals for tournament
    (if (chance bprob)
      (first (sort tpop (lambda (x y) (< (car x) (car y)))))
      (first (shuffle tpop)))))

;; (: get-coords-from-file ( String String Number -> (Listof (Listof Number)) ))
(define (get-coords-from-file filename sep [start-ind 0])
  (let* ([lines      (file->lines filename)]
         [tokens     (map    (lambda (x) (string-split x sep))        lines)]
         [ntokens    (filter (lambda (x) (not (null? x)))             tokens)]
         [str-coords (map    (lambda (x) (take (drop x start-ind) 2)) ntokens)]
         [num-coords (map    (lambda (x) (map string->number x))      str-coords)])
    num-coords))

(define (calc-fitness individual)
  (let ([outside (line-distance (car individual) (car (reverse individual)))]
        [inside (foldl (lambda (curr fpt)
                         (cons curr (list (+ (cadr fpt) (line-distance curr (car fpt))))))
                       (list (car individual) 0)
                       (cdr individual))])
    (+ outside (cadr inside))))        ; Add the internal tour and the first and last point

(define (create-random-tour domain)
  (lambda (_) (shuffle domain)))

;; TODO change to command line arg
(define coords (get-coords-from-file "berlin52.txt" " " 1))









(define population
  (initialize-population (create-random-tour coords) 0 10))

(newline) (newline)

(for ([c (map cons (map calc-fitness population) population)])
     (display (car c)) (newline))

(newline) (newline)

(for ([c (map cons (map calc-fitness population) population)])
     (display (car c)) (newline))


(selection-tournament 10 0.65 (zip (map calc-fitness population) population))

;(define (tsp-crossover )
;  (let ([p1 (selection-tournament )])))
;
;; Drawing utils
;; :  ( (Listof (Listof Number)) -> Number )
(define (get-min-x coords) (car (argmin car  coords)))
(define (get-max-x coords) (car (argmax car  coords)))
(define (get-min-y coords) (cadr (argmin cadr coords)))
(define (get-max-y coords) (cadr (argmax cadr coords)))

;; : Number
(define xmin (get-min-x coords))
(define xmax (get-max-x coords))
(define ymin (get-min-y coords))
(define ymax (get-max-y coords))
;; DRAWING ;;


;; Display points
;(start-gui)
;(define (loop coords)
;  (let ((t (current-milliseconds)))
;    (update-tour-view (world coords xmin xmax ymin ymax))
;    (sleep 1/25)
;    (loop (shuffle coords))))
;(loop coords)
