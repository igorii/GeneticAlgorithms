#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "util.rkt"
         "general-ga.rkt"
         "window.rkt")

;; Utils
(define (fpop->pop  fpop) (map cdr fpop))
(define (fpop->fits fpop) (map car fpop))
(define (get-unused l used) (filter (lambda (x) (not (hash-has-key? used x))) l))
(define (get-coords-from-file filename sep [start-ind 0])
  (let* ([lines      (file->lines filename)]
         [tokens     (map    (lambda (x) (string-split x sep))        lines)]
         [ntokens    (filter (lambda (x) (not (null? x)))             tokens)]
         [str-coords (map    (lambda (x) (take (drop x start-ind) 2)) ntokens)]
         [num-coords (map    (lambda (x) (map string->number x))      str-coords)])
    num-coords))
(define (line-distance p1 p2)
  (sqrt (+ (sqr (- (car  p1) (car  p2)))
           (sqr (- (cadr p1) (cadr p2))))))
;; End utils

(define (crossover-partially-mapped p1 p2 strlen) 
  (define (phase1 p1 p2 a b new-tour used-map)
    (define (loop pos new-tour p1 p2 used-map)
      (cond [(null? p1) (list new-tour used-map)]
            [(null? p2) (list new-tour used-map)]
            [(> pos b)  (loop (add1 pos) (append new-tour (list null)) (cdr p1) (cdr p2) used-map)]
            [(< pos a)  (loop (add1 pos) (append new-tour (list null)) (cdr p1) (cdr p2) used-map)]
            [else (loop (add1 pos) (append new-tour (list (car p1)))
                        (cdr p1) (cdr p2) (hash-set used-map (car p1) #t))]))
    (loop 0 new-tour p1 p2 used-map))


  (define (phase2 p1 p2 a b new-tour used-map)
    (define (loop pos src dest p1 p2 used-map)
      (cond [(null? p1) (list dest used-map)]
            [(null? p2) (list dest used-map)]
            [(and (<= pos b) (>= pos a)) 
             (loop (add1 pos) (cdr src) (append dest (list (car src))) 
                   (cdr p1) (cdr p2)
                   used-map)]
            [(hash-has-key? used-map (car p2)) (loop (add1 pos) (cdr src)
                                                     (append dest (list (car src))) 
                                                     (cdr p1) (cdr p2)
                                                     used-map)]
            [else (loop (add1 pos) (cdr src) 
                        (append dest (list (car p2))) (cdr p1) (cdr p2) 
                        (hash-set used-map (car p2) #t))]))
    (loop 0 new-tour '() p1 p2 used-map))


  (define (phase3-2 p1 p2 new-tour used-map)
    (define (loop unused new-tour acc)
      (cond [(null? new-tour) acc]
            [(null? (car new-tour)) 
             (loop (cdr unused) (cdr new-tour) (append acc (list (car unused))))]
            [else (loop unused  (cdr new-tour) (append acc (list (car new-tour))))]))
    (loop (get-unused p2 used-map) new-tour '()))

  (let* ([r1           (random strlen)]       ; Create two random points for crossover
         [r2           (random strlen)]
         [point-a      (if (> r1 r2) r2 r1)]  ; Get the min point for corssover
         [point-b      (if (> r1 r2) r1 r2)]  ; Get the max point for crossover
         [used-map     #hash()]               ; Store used cities here to avoid linear lookup
         [new-tour     '()]
         [post-phase1  (phase1 p1 p2 point-a point-b new-tour used-map)]
         [post-phase2  (phase2 p1 p2 point-a point-b (car post-phase1) (cadr post-phase1))]
         [post-phase3  (phase3-2 p1 p2 (car post-phase2) (cadr post-phase2))])
    (begin
      ;(display "Parent 1: ") (display p1) (newline)
      ;(display "Parent 2: ") (display p2) (newline)
      ;(display "Points  : ") (display (list point-a point-b)) (newline)
      ;(display "Post p1 : ") (display (car post-phase1)) (newline)
      ;(display "Post p2 : ") (display (car post-phase2)) (newline)
      ;(display "Child   : ") (display post-phase3) (newline)
      post-phase3)))

(define (crossover-injection p1 p2)      null)
(define (crossover-order p1 p2)          null)

;; two mutation operators
(define (mutation-1 individual) null)
(define (mutation-2 individual) null)

;; Two of
(define (selection-ranked pop)   null)
(define (selection-roulette pop) null)

;; selection-tournament : ( Number -> Real -> (Listof '( Number '( Number Number ))) )
(define (selection-tournament tsize bprob fpop)
  (let* ([tpop (take (shuffle fpop) tsize)])                   ; Select the individuals for tournament
    (if (chance bprob)
      (first (sort tpop (lambda (x y) (< (car x) (car y)))))   ; bprob % of the time take the best
      (first (shuffle tpop)))))                                ; Otherwise take a random one

(define (fill-nulls with l acc)
  (cond [(null? l) acc]
        [(null? (car l)) (fill-nulls (cdr with) (cdr l) (append acc (list (car with))))]
        [else (fill-nulls with (cdr l) (append acc (list (car l))))]))

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

(define (tsp-crossover tsize bprob fpop strlen)
  (let* ([p1 (selection-tournament tsize bprob fpop)]
         [p2 (selection-tournament tsize bprob fpop)]
         [candidate ((random-crossover) (cdr p1) (cdr p2) strlen)])
    ((random-mutation) candidate strlen)))

(define (create-new-pop fpop tsize bprob popsize strlen)
  (map (lambda (_) (tsp-crossover tsize bprob fpop strlen)) (range 0 popsize)))

(define (mutation-inversion individual strlen)
  (let* ([r1     (random strlen)]
         [r2     (random strlen)]
         [a      (if (< r1 r2) r1 r2)]
         [b      (if (< r1 r2) r2 r1)]
         [start  (take individual a)]
         [middle (take (drop individual a) (- b a))]
         [end    (drop individual (+ a (- b a)))])
    (append start (reverse middle) end)))


(define (mutation-exchange individual strlen)
  (let* ([v (list->vector individual)]
         [a (random strlen)]
         [b (random strlen)]
         [c (vector-ref v a)])
    (vector-set! v a (vector-ref v b))
    (vector-set! v b c)
    (vector->list v)))


(define (random-mutation)
  (let* ([ms (vector mutation-exchange mutation-inversion mutation-scramble)]
  ;(let* ([ms (vector mutation-exchange mutation-inversion)]
         [r (random (vector-length ms))])
    (vector-ref ms r)))

(define (random-crossover)
  (let* ([cs (vector crossover-position-based crossover-partially-mapped)]
         [r (random (vector-length cs))])
    (vector-ref cs r)))


;(define (crossover-partially-mapped p1 p2 strlen) 
(define (crossover-position-based p1 p2 strlen)
  (define (get-from-p1 p1 acc used)
    (cond [(null? p1) (list acc used)]
          [(chance 0.5) (get-from-p1 (cdr p1) (append acc (list (car p1))) (hash-set used (car p1) #t))]
          [else         (get-from-p1 (cdr p1) (append acc (list null)) used)]))
  (let* ([post-p1 (get-from-p1 p1 '() #hash())]
         [unused (get-unused p2 (cadr post-p1))])
    (fill-nulls unused (car post-p1) '())))



(define (mutation-scramble individual strlen)
  (let* ([r1 (random strlen)]
         [r2 (random strlen)]
         [a  (if (< r1 r2) r1 r2)]
         [b  (if (< r1 r2) r2 r1)]
         [start (take individual a)]
         [mid   (take (drop individual a) (- b a))]
         [end   (drop individual (+ a (- b a)))])
    (append start (shuffle mid) end)))








(define (mutation-displacement individual strlen)
  ;; Select a random subtour (between points and and b)
  ;; Select a random insertion point not in subtour
  ;; Move the subtour to within the insertion point
  null)



;; Drawing utils
;; :  ( (Listof (Listof Number)) -> Number )
(define (get-min-x coords) (car  (argmin car  coords)))
(define (get-max-x coords) (car  (argmax car  coords)))
(define (get-min-y coords) (cadr (argmin cadr coords)))
(define (get-max-y coords) (cadr (argmax cadr coords)))

;; : Number
(define xmin (get-min-x coords))
(define xmax (get-max-x coords))
(define ymin (get-min-y coords))
(define ymax (get-max-y coords))

(define popsize 150)
(define population (initialize-population (create-random-tour coords) 0 popsize))
(define strlen (length (car population)))
(define (loop oldpop)
  (let* ([fits (map calc-fitness oldpop)]
         [fpop (zip fits oldpop)]
         [best (argmin car fpop)])
    (update-tour-view (world (cdr best) xmin xmax ymin ymax))
    (display (car best)) (newline)
    ;(loop (create-new-pop fpop 20 0.65 popsize strlen))))
    (loop (append (cdr (create-new-pop fpop 15 0.50 popsize strlen)) (list (cdr best))))))

(start-gui)
(loop population)
