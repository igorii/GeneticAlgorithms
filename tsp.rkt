#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "util.rkt"
         "general-ga.rkt"
         "window.rkt")

;; *****
;; Utils
;; *****

(define (fpop->pop  fpop) (map cdr fpop))
(define (fpop->fits fpop) (map car fpop))
(define (get-unused l used) (filter (lambda (x) (not (hash-has-key? used x))) l))
(define (fill-nulls with l acc)
  (cond [(null? l) acc]
        [(null? (car l)) (fill-nulls (cdr with) (cdr l) (append acc (list (car with))))]
        [else (fill-nulls with (cdr l) (append acc (list (car l))))]))
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


(define (crossover-injection p1 p2)      null)
(define (crossover-order p1 p2)          null)

;; two mutation operators
(define (mutation-1 individual) null)
(define (mutation-2 individual) null)


(define (calc-fitness individual)
  (let ([outside (line-distance (car individual) (car (reverse individual)))]
        [inside (foldl (lambda (curr fpt)
                         (cons curr (list (+ (cadr fpt) (line-distance curr (car fpt))))))
                       (list (car individual) 0)
                       (cdr individual))])
    (+ outside (cadr inside))))        ; Add the internal tour and the first and last point

(define (create-random-tour domain)
  (lambda (_) (shuffle domain)))

(define (create-new-pop fpop tsize bprob popsize strlen)
  (map (lambda (_) (tsp-crossover tsize bprob fpop strlen)) (range 0 popsize)))

;; **********
;; Crossovers
;; **********

(define (random-crossover)
  (let* ([cs (vector crossover-position-based crossover-partially-mapped)]
         [r (random (vector-length cs))])
    (vector-ref cs r)))

(define (tsp-crossover tsize bprob fpop strlen)
  (let* ([select (random-selection)]
         [p1 (select tsize bprob fpop)]
         [p2 (select tsize bprob fpop)]
         [candidate ((random-crossover) (cdr p1) (cdr p2) strlen)])
    ((random-mutation) candidate strlen)))

(define (crossover-partially-mapped p1 p2 strlen) 
  (define (phase1 p1 p2 a b tour used-map)
    (define (loop pos tour p1 p2 used-map)
      (cond [(null? p1) (list tour used-map)]
            [(> pos b)  (loop (add1 pos) (append tour (list null)) (cdr p1) (cdr p2) used-map)]
            [(< pos a)  (loop (add1 pos) (append tour (list null)) (cdr p1) (cdr p2) used-map)]
            [else (loop (add1 pos) (append tour (list (car p1)))
                        (cdr p1) (cdr p2) (hash-set used-map (car p1) #t))]))
    (loop 0 tour p1 p2 used-map))

  (define (phase2 p1 p2 a b tour used-map)
    (define (loop pos src dest p1 p2 used-map)
      (cond [(null? p1) (list dest used-map)]
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
    (loop 0 tour '() p1 p2 used-map))

  (let* ([r1           (random strlen)]       ; Create two random points for crossover
         [r2           (random strlen)]
         [point-a      (if (> r1 r2) r2 r1)]  ; Get the min point for corssover
         [point-b      (if (> r1 r2) r1 r2)]  ; Get the max point for crossover
         [used-map     #hash()]               ; Store used cities here to avoid linear lookup
         [tour         '()]
         [post-phase1  (phase1 p1 p2 point-a point-b tour used-map)]
         [post-phase2  (phase2 p1 p2 point-a point-b (car post-phase1) (cadr post-phase1))]
         [post-phase3  (fill-nulls (get-unused p2 (cadr post-phase2)) (car post-phase2) '())])
    post-phase3))

(define (crossover-position-based p1 p2 strlen)
  (define (get-from-p1 p1 acc used)
    (cond [(null? p1) (list acc used)]
          [(chance 0.5) (get-from-p1 (cdr p1) (append acc (list (car p1))) (hash-set used (car p1) #t))]
          [else         (get-from-p1 (cdr p1) (append acc (list null)) used)]))
  (let* ([post-p1 (get-from-p1 p1 '() #hash())]
         [unused (get-unused p2 (cadr post-p1))])
    (fill-nulls unused (car post-p1) '())))

;; **********
;; Selections
;; **********

(define (random-selection)
  (let* ([ss (vector selection-tournament)]
         [r (random (vector-length ss))])
    (vector-ref ss r)))

(define (selection-ranked pop)   null)
(define (selection-roulette pop) null)

(define (selection-tournament tsize bprob fpop)
  (let* ([tpop (take (shuffle fpop) tsize)])                   ; Select the individuals for tournament
    (if (chance bprob)
      (first (sort tpop (lambda (x y) (< (car x) (car y)))))   ; bprob % of the time take the best
      (first (shuffle tpop)))))                                ; Otherwise take a random one

;; *********
;; Mutations
;; *********

(define (random-mutation)
  ;(let* ([ms (vector mutation-exchange mutation-inversion mutation-scramble)]
  (let* ([ms (vector mutation-exchange mutation-inversion)]
         [r (random (vector-length ms))])
    (vector-ref ms r)))

(define (mutation-insertion) null)

(define (mutation-displaced-inversion) null)

(define (mutation-inversion individual strlen)
  (let* ([r1     (random strlen)]
         [r2     (random strlen)]
         [a      (if (< r1 r2) r1 r2)]
         [b      (if (< r1 r2) r2 r1)]
         [start  (take individual a)]
         [middle (take (drop individual a) (- b a))]
         [end    (drop individual (+ a (- b a)))])
    (append start (reverse middle) end)))

(define (mutation-scramble individual strlen)
  (let* ([r1 (random strlen)]
         [r2 (random strlen)]
         [a  (if (< r1 r2) r1 r2)]
         [b  (if (< r1 r2) r2 r1)]
         [start (take individual a)]
         [mid   (take (drop individual a) (- b a))]
         [end   (drop individual (+ a (- b a)))])
    (append start (shuffle mid) end)))

(define (mutation-exchange individual strlen)
  (let* ([v (list->vector individual)]
         [a (random strlen)]
         [b (random strlen)]
         [c (vector-ref v a)])
    (vector-set! v a (vector-ref v b))
    (vector-set! v b c)
    (vector->list v)))

(define (mutation-displacement individual strlen)
  ;; Select a random subtour (between points and and b)
  ;; Select a random insertion point not in subtour
  ;; Move the subtour to within the insertion point
  null)

;; **************
;; Initialization
;; **************

(define coords (get-coords-from-file "berlin52.txt" " " 1))
(define popsize 120)
(define population (initialize-population (create-random-tour coords) 0 popsize))
(define strlen (length (car population)))
(define (loop oldpop)
  (let* ([fits (map calc-fitness oldpop)]
         [fpop (zip fits oldpop)]
         [best (argmin car fpop)])
    (update-tour-view (world (cdr best) xmin xmax ymin ymax))
    (display (car best)) (newline)
    (loop (append (cdr (create-new-pop fpop 20 0.65 popsize strlen)) (list (cdr best))))))

;; *******
;; Drawing
;; *******
(define xmin (car  (argmin car  coords)))
(define xmax (car  (argmax car  coords)))
(define ymin (cadr (argmin cadr coords)))
(define ymax (cadr (argmax cadr coords)))

;; *****
;; Start
;; *****

(start-gui)
(loop population)
