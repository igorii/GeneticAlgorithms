#lang racket

(require racket/gui)
(require "util.rkt"
         "general-ga.rkt"
         "window.rkt")

;; **********
;; Structures
;; **********

(struct args     (file sep col) #:transparent)
(struct settings (restart pause mutation crossover) #:transparent)

;; ************
;; Command Line
;; ************

(define cargs (command-line #:args (file sep col)
    (args file sep (string->number col))))

;; *************
;; Global params
;; *************

(define *settings* (settings #f #t "Random" "Random"))
(define *ga-thread* null)
(define *popsize* 100)
(define *rcoords* (map (λ (_) (list (random 1000) (random 1000))) (range 0 40)))
(define *coords* (get-coords-from-file (args-file cargs) (args-sep cargs) (args-col cargs)))

;; *****
;; Utils
;; *****

(define (fpop->pop  fpop) (map cdr fpop))
(define (fpop->fits fpop) (map car fpop))
(define (get-unused l used) (filter (lambda (x) (not (hash-has-key? used x))) l))
(define (line-distance p1 p2)
  (sqrt (+ (sqr (- (car  p1) (car  p2)))
           (sqr (- (cadr p1) (cadr p2))))))

;; *******
;; Fitness
;; *******

(define (calc-fitness individual)
  (let ([outside (line-distance (car individual) (car (reverse individual)))]
        [inside (foldl (lambda (curr fpt)
                         (cons curr (list (+ (cadr fpt) (line-distance curr (car fpt))))))
                       (list (car individual) 0)
                       (cdr individual))])
    (+ outside (cadr inside))))        ; Add the internal tour and the first and last point

;; **********
;; Crossovers
;; **********

(define (random-crossover)
  (let* ([cs (vector crossover-position-based crossover-partially-mapped)]
         [r (random (vector-length cs))])
    (vector-ref cs r)))

(define (tsp-crossover tsize bprob fpop nbest strlen popsize)
  (let* ([select (random-selection tsize bprob nbest popsize)]
         [p1        (select fpop)]
         [p2        (select fpop)]
         [candidate ((random-crossover) (cdr p1) (cdr p2) strlen)])
    ((random-mutation) candidate strlen)))

(define (crossover-position-based p1 p2 strlen)
  (define (get-from-p1 p1 acc used)
    (cond [(null? p1) (list acc used)]
          [(chance 0.5) (get-from-p1 (cdr p1) (append acc (list (car p1))) (hash-set used (car p1) #t))]
          [else         (get-from-p1 (cdr p1) (append acc (list null)) used)]))
  (let* ([post-p1 (get-from-p1 p1 '() #hash())]
         [unused (get-unused p2 (cadr post-p1))])
    (fill-nulls unused (car post-p1) '())))

(define (crossover-injection p1 p2) null)
(define (crossover-order p1 p2) null)

(define (crossover-partially-mapped p1 p2 strlen) 
  (define (phase1 p1 p2 a b tour used-map)
    (define (loop pos tour p1 p2 used-map)
      (cond [(null? p1) (list tour used-map)]
            [(> pos b)  (loop (add1 pos) (append tour (list null)) (cdr p1) (cdr p2) used-map)]
            [(< pos a)  (loop (add1 pos) (append tour (list null)) (cdr p1) (cdr p2) used-map)]
            [else       (loop (add1 pos) (append tour (list (car p1))) (cdr p1) (cdr p2) (hash-set used-map (car p1) #t))]))
    (loop 0 tour p1 p2 used-map))

  (define (phase2 p1 p2 a b tour used-map)
    (define (loop pos src dest p1 p2 used-map)
      (cond [(null? p1) (list dest used-map)]
            [(and (<= pos b) (>= pos a)) 
             (loop (add1 pos) (cdr src) (append dest (list (car src))) (cdr p1) (cdr p2) used-map)]
            [(hash-has-key? used-map (car p2)) 
             (loop (add1 pos) (cdr src) (append dest (list (car src))) (cdr p1) (cdr p2) used-map)]
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

;; **********
;; Selections
;; **********

(define (random-selection tsize bprob ranked-base popsize)
  ;(let* ([ss (vector (selection-tournament tsize bprob) (selection-ranked ranked-base popsize))]
  (let* ([ss (vector (selection-ranked ranked-base popsize))]
         [r  (random (vector-length ss))])
    (vector-ref ss r)))

(define (selection-tournament tsize bprob)
  (lambda (fpop)
    (let* ([tpop (take (shuffle fpop) tsize)])                   ; Select the individuals for tournament
      (if (chance bprob)
        (first (sort tpop (lambda (x y) (< (car x) (car y)))))   ; bprob % of the time take the best
        (first (shuffle tpop))))))                               ; Otherwise take a random one

(define (selection-ranked base u)
  (define (select r probs fpop last)
    (if (> r (car probs)) last
      (select r (cdr probs) (cdr fpop) (car fpop))))

  (lambda (fpop) 
    (let* ([sorted (sort fpop (λ (a b) (< (car a) (car b))))]
           [probs  (map (λ (x) (+ (/ (- 2 base) u)
                                 (/ (* (- base 1) (* 2 x)) (* u (- u 1)))))
                        (range 1 (add1 u)))]
           [sprobs (scan + 0 probs)]
           [r      (random)])
      (display (foldl + 0 probs)) (display "        ") (display (car (reverse sprobs)))(newline)
      (select r probs sorted (car sorted)))))

;; *********
;; Mutations
;; *********

(define (random-mutation)
  (let* ([ms (vector mutation-insertion mutation-exchange mutation-inversion)]
         ;(let* ([ms (vector mutation-insertion mutation-exchange mutation-inversion mutation-scramble)]
         [r (random (vector-length ms))])
    (vector-ref ms r)))

(define (mutation-displaced-inversion) null)
(define (mutation-displacement individual strlen) null)

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

(define (mutation-insertion individual strlen)
  (let* ([a (random strlen)]
         [b (random strlen)]
         [remd (append (take individual a) (drop individual (add1 a)))]
         [v (car (drop individual a))])
    (insert-at remd b v)))

;; **************
;; Initialization
;; **************

(define (create-random-tour domain)
  (lambda (_) (shuffle domain)))

(define (create-new-pop fpop tsize bprob nbest popsize strlen)
  (map (lambda (_) (tsp-crossover tsize bprob fpop nbest strlen popsize)) (range 0 popsize)))

;; ***
;; GUI
;; ***

(define top-row-panel    (new horizontal-panel% [parent frame] [alignment (list 'center 'center)]))
(define middle-row-panel (new horizontal-panel% [parent frame] [alignment (list 'center 'center)]))

(define pause-btn (new button% [parent top-row-panel]
                       [label "Play/Pause"]
                       [callback (lambda (button event) 
                                   (if (not (thread? *ga-thread*))
                                     (new-run)
                                     (set! *settings* (struct-copy settings *settings* [pause (not (settings-pause *settings*))]))))]))

(define restart-btn (new button% [parent top-row-panel]
                         [label "Restart"]
                         [callback (lambda (button event) (new-run))]))

(define mutation-choice (new choice% [parent middle-row-panel]
                             [label "Mutation"]
                             [choices (list "Random" "Insertion" "Inversion" "Exchange" "Scramble")]
                             [callback (lambda (choice event) 1)]))
;(display (send choice get-string-selection)) (newline))])

(define crossover-choice (new choice% [parent middle-row-panel]
                              [label "Crossover"]
                              [choices (list "Random" "Ranked" "Tournament")]
                              [callback (lambda (choice event) 1)]))
;(display (send choice get-string-selection)) (newline))])

(define (new-run)
  (let* ([population (initialize-population (create-random-tour *coords*) 0 *popsize*)]
         [xmin   (car  (argmin car  (car population)))]
         [xmax   (car  (argmax car  (car population)))]
         [ymin   (cadr (argmin cadr (car population)))]
         [ymax   (cadr (argmax cadr (car population)))]
         [strlen (length (car population))])
    (if (thread? *ga-thread*) (kill-thread *ga-thread*) null)
    (set! *settings* (struct-copy settings *settings* [pause #f]))
    (set! *ga-thread* (thread (lambda () (loop "main" #t population *popsize* strlen (world null xmin xmax ymin ymax)))))))

;; *******
;; Drawing
;; *******

(define (loop name render oldpop popsize strlen w)
  (if (settings-pause *settings*) 
    (loop name render oldpop popsize strlen w)
    (let* ([fits  (map calc-fitness oldpop)]
           [fpop  (zip fits oldpop)]
           [best  (argmin car fpop)])
      (update-tour-view (number->string (car best)) (struct-copy world w [points (cdr best)]))
      (loop name render (append (cdr (create-new-pop fpop 7 0.65 8 popsize strlen)) (list (cdr best))) popsize strlen w))))

;; *****
;; Start
;; *****

(start-gui)
(new-run)
