#lang racket

(require racket/match)
(require racket/gui)
(require "util.rkt"
         "general-ga.rkt"
         "window.rkt")

;; **********
;; Structures
;; **********

;; Command line arguments
(struct args (file sep col) #:transparent)

;; Genetic Algorithm settings
(struct settings (restart pause mutation crossover selection mutation-prob) #:transparent)

;; Selection algorithm settings
;;     This is held in a struct to give an interface to select functions
(struct s-select (tsize bprob ranked-base popsize) #:transparent)

;; ************
;; Command Line
;; ************

(define cargs (command-line #:args (file sep col)
                            (args file sep (string->number col))))

;; *************
;; Global params
;; *************

(define *verbose* #f)

;; Looping var
(define *i* 0)

;; A handle to the thread responsible for looping and rendering the genetic algorithm
(define *ga-thread*         null)

;; The population size used in the genetic algorithm
(define *popsize*           50)

;; The tournament size used in tournement selection
(define *tournament-size*   8)

;; The probability that the best individual will be chosen in *tournament selection*
(define *prob-best*         0.85)

;; The expected number of children that the best individual will create when using *ranked selection*
(define *num-best-children* 2)

;; Settings for the current run. This is updated by the GUI layer with the desired choices
(define *settings*          (settings #f #t "Random" "Random" "Random" 0.5))

;; Coordinate system identifiers for a random tour and the Berlin52 problem
;(define *coords*           (map (lambda (_) (list (random 1000) (random 1000))) (range 0 20)))
(define *coords*            (get-coords-from-file (args-file cargs) (args-sep cargs) (args-col cargs)))

;; *****
;; Utils
;; *****

(define (fpop->pop  fpop) (map cdr fpop))
(define (fpop->fits fpop) (map car fpop))
(define (get-unused l used) (filter (lambda (x) (not (hash-has-key? used x))) l))
(define (line-distance p1 p2)
  (sqrt (+ (sqr (- (car  p1) (car  p2)))
           (sqr (- (cadr p1) (cadr p2))))))
(define (convert-to-nums individual)
  (let ([h (make-hash (zip *coords* (range 0 (length *coords*))))])
    (map (lambda (x) (hash-ref h x null)) individual)))
(define (add-points-at-indicies l vs ps)
  (let* ([add-1 (insert-at l (car ps) (car vs))]
         [add-2 (insert-at add-1 (+ 2 (cadr ps)) (cadr vs))]) ;; Add 2 to account for inclusivity
    add-2))

;; Return a range within 0 and the given max
(define (get-range m)
  (let* ([r1           (random m)]
         [r2           (random m)]
         [point-a      (if (> r1 r2) r2 r1)]
         [point-b      (if (> r1 r2) r1 r2)])
    (list point-a point-b)))

;; *******
;; Fitness
;; *******

;; The fitness of a tour is the distance traveled along the tour. This is calculated
;; by summing the distances between the points, and adding the distance between the 
;; endpoints. The distance between the endpoints is added to accomodate for a return
;; to the origin city.
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

;; Determine the crossover function associated with the given label
(define (lbl->crossover-fn lbl)
  (cond [(eq? lbl "Random")           (random-crossover)]
        [(eq? lbl "Position Based")   crossover-position-based]
        [(eq? lbl "Partially Mapped") crossover-partially-mapped]))

;; Return a random crossover function
(define (random-crossover)
  (let* ([cs (vector crossover-position-based crossover-partially-mapped)]
         [r (random (vector-length cs))])
    (vector-ref cs r)))

;; Select two individuals from the given population with replacement and return
;; a child of the two selected parents. The child will be mutated with probability
;; specified by `mutation-rate`
(define (tsp-crossover ranked settings tsize bprob fpop nbest strlen popsize)
  (let* ([select-settings (s-select tsize bprob nbest popsize)]
         [sfn       (lbl->selection    (settings-selection settings))]
         [cfn       (lbl->crossover-fn (settings-crossover settings))]
         [mfn       (lbl->mutation-fn  (settings-mutation  settings))]
         [p1        (sfn fpop select-settings ranked)]
         [p2        (sfn fpop select-settings ranked)]
         [candidate (cfn (cdr p1) (cdr p2) strlen)])
    (if (chance (settings-mutation-prob settings))
      (mfn candidate strlen)
      candidate)))

;; Perform a position-based crossover on the given two parents.
;; This will decide with a probability of 0.5 for each element in p1 whether
;; it should be included in the child. If yes, it is placed in the child at the
;; index it appears in p1. Otherwise, a null is added temprarily, and later filled
;; in by the unused elements in order of appearance in p2.
(define (crossover-position-based p1 p2 strlen)
  (define (get-from-p1 p1 acc used)
    (cond [(null? p1)   (list acc used)]
          [(chance 0.5) (get-from-p1 (cdr p1) (append acc (list (car p1))) (hash-set used (car p1) #t))]
          [else         (get-from-p1 (cdr p1) (append acc (list null)) used)]))
  (let* ([post-p1 (get-from-p1 p1 '() #hash())]
         [unused (get-unused p2 (cadr post-p1))]
         [child (fill-nulls unused (car post-p1) '())])
    (if *verbose* 
      (begin
        (display "Position Based Crossover")      (newline)
        (display "Parent 1: ") (display (convert-to-nums p1))       (newline)
        (display "Parent 2: ") (display (convert-to-nums p2))       (newline)
        (display "From p1 : ") (display (map (lambda (x) (if (null? x) #\_ x)) (convert-to-nums (car post-p1))))  (newline)
        (display "Child   : ") (display (convert-to-nums child))    (newline))
      null)
    child))

(define (crossover-injection p1 p2) null)
(define (crossover-order p1 p2) null)

;; Perform a partially-mapped crossover on the given two parents. A region will
;; be selected within the parents. Elements within the region will be added to the
;; child at the index they appear in p1. Unused elements will be added from p2 in
;; the order that they appear in p2.
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

  (let* ([points       (get-range strlen)]
         [post-phase1  (phase1 p1 p2 (car points) (cadr points) '() #hash())]
         [post-phase2  (phase2 p1 p2 (car points) (cadr points) (car post-phase1) (cadr post-phase1))]
         [post-phase3  (fill-nulls (get-unused p2 (cadr post-phase2)) (car post-phase2) '())])
    (if *verbose* 
      (begin
        (display "Partially Mapped Crossover")       (newline)
        (display "Parent 1: ") (display (convert-to-nums p1))          (newline)
        (display "Parent 2: ") (display (convert-to-nums p2))          (newline)
        (display "Points  : ") (display points)      (newline)
        (display "Child   : ") (display (add-points-at-indicies (convert-to-nums post-phase3) (list #\_ #\_) points)) (newline))
      null)
    post-phase3))


;; **********
;; Selections
;; **********

(define (lbl->selection lbl)
  (cond [(eq? lbl "Random")     (random-selection)]
        [(eq? lbl "Tournament") selection-tournament]
        [(eq? lbl "Roulette")   selection-roulette]
        [(eq? lbl "Ranked")     selection-ranked]))

(define (random-selection)
  (let* ([ss (vector selection-tournament selection-ranked)]
         [r  (random (vector-length ss))])
    (vector-ref ss r)))

(define (selection-tournament fpop select-settings _)
  (let* ([tpop (take (shuffle fpop) (s-select-tsize select-settings))])   ; Select the individuals for tournament
    (if (chance (s-select-bprob select-settings))
      (first (sort tpop (lambda (x y) (< (car x) (car y)))))              ; bprob % of the time take the best
      (first (shuffle tpop)))))                                           ; Otherwise take a random one

(define (rank-pop select-settings fpop popsize)
  (define base (s-select-ranked-base select-settings))
  (define u    (s-select-popsize     select-settings))

  ;; Assign a probability based on the following:
  ;;     @see slide 25 - howgaswork1.pdf
  ;;
  ;;         (2 - s)     2i(s - 1)
  ;; P(i) =  -------  +  ---------,  from 0 to (u - 1)
  ;;            u        u(u - 1)
  ;;
  (define (assign i)
    (+ (/ (- 2 base) u)
       (/ (* (- base 1) (* 2 i)) (* u (- u 1)))))

  ;; Assigns a probability based on the following:
  ;;
  ;;          2*i
  ;; P(i) = --------
  ;;        u(u + 1)
  ;;
  (define (assign2 i)
    (/ (* 2 i)
       (* u (add1 u))))

  ;; Assign a probability based on the function given in the assignment description:
  ;; P(i) = p^r
  (define (assign3 i)
    (expt 0.5 i))

  ;; TODO - move sorting to main loop before selection (so it done only once per generation)
  (let* ([sorted (sort fpop (lambda (a b) (< (car a) (car b))))]  ; Sort the population
         ;; Reverse the probabilities since we are minimizng the scores
         [probs  (map assign (reverse (range 0 u)))]       ; Assign rank based probabilities
         [sprobs (scan + 0 probs)])                        ; Scan addition over the probabilities
    (if *verbose* (printf "\n    Probs: ~a\n     Total: ~a\n" probs (car (reverse sprobs))) null)
    (list sorted sprobs)))

(define (selection-ranked fpop select-settings ranked)
  (define (select r probs fpop)
    (if (< r (car probs))
      (car fpop)
      (select r (cdr probs) (cdr fpop))))

  ;; TODO - move sorting to main loop before selection (so it done only once per generation)
  (let* ([r        (random)]                               ; Choose a random individual
         [selected (select r (cadr ranked) (car ranked))])              ; Determine which individual was chosen
    selected))

(define (total-fitness fs) (foldl + 0 fs))

;; (: selection-roulette ( (Listof Chromosome) (Listof Number) -> (Listof Chromosome) ))
;; from slide 19
(define (selection-roulette fpop select-settings _)
  (define (loop wp partial-sum fpop)
    (let ([next-fitness (+ partial-sum (caar fpop))])
      (cond [(null? (cdr fpop))  (car fpop)]
            [(> next-fitness wp) (car fpop)]
            [else                (loop wp next-fitness (cdr fpop))])))
  (loop (* (random) (total-fitness (fpop->fits fpop))) 0 fpop))

;; *********
;; Mutations
;; *********

;; Returns a mutation operation from a given label
(define (lbl->mutation-fn lbl)
  (cond [(eq? lbl "Random") (random-mutation)]
        [(eq? lbl "Inversion") mutation-inversion]
        [(eq? lbl "Scramble")  mutation-scramble]
        [(eq? lbl "Insertion") mutation-insertion]
        [(eq? lbl "Exchange")  mutation-exchange]))

;; Returns a random mutation operation
(define (random-mutation)
  (let* ([ms (vector mutation-insertion mutation-exchange mutation-inversion mutation-scramble)]
         [r (random (vector-length ms))])
    (vector-ref ms r)))

;; Unused
(define (mutation-displaced-inversion) null)
(define (mutation-displacement individual strlen) null)

;; Private helper mutation for performing an operation on an inner sublist
(define (inner-mutation op individual strlen)
  (match-define (list a b) (get-range strlen))
  (let* ([start  (take individual a)]
         [middle (take (drop individual a) (- b a))]
         [end    (drop individual (+ a (- b a)))])
    (if *verbose* (printf "(~a, ~a)  " a b) null)
    (append start (op middle) end)))

;; Mutate an individual by reversing an inner sublist
(define (mutation-inversion individual strlen)
  (inner-mutation reverse individual strlen))

;; Mutate an individual by shuffling an inner sublist
(define (mutation-scramble individual strlen)
  (inner-mutation shuffle individual strlen))

;; Mutate an individual by switching two of its elements
(define (mutation-exchange individual strlen)
  (let* ([v (list->vector individual)]
         [a (random strlen)]
         [b (random strlen)]
         [c (vector-ref v a)])
    (if *verbose* (printf "(~a, ~a)  " a b) null)
    (vector-set! v a (vector-ref v b))
    (vector-set! v b c)
    (vector->list v)))

;; Mutate an individual by moving a sublist to another position within
;; the tour
(define (mutation-insertion individual strlen)
  (let* ([a (random strlen)]
         [b (random strlen)]
         [remd (append (take individual a) (drop individual (add1 a)))]
         [v (car (drop individual a))])
    (if *verbose* (printf "(~a, ~a)  " a b) null)
    (insert-at remd b v)))

;; **************
;; Initialization
;; **************

;; Create a random tour simpyl by shuffling the possible cities
(define (create-random-tour domain)
  (lambda (_) (shuffle domain)))

;; Create a new population by creating popsize many new tours
;; New tours are added to a hash map to avoid introducing duplicates
(define (create-new-pop settings fpop tsize bprob nbest popsize strlen)
  ;; Sort and rank before crossover
  (let ([ranked (rank-pop (s-select tsize bprob nbest popsize) fpop popsize)])
    (map (lambda (_) (tsp-crossover ranked settings tsize bprob fpop nbest strlen popsize))
         (range 0 popsize))))

;; ***
;; GUI
;; ***

(define option-panel     (new vertical-panel% [parent main-panel]))
(define middle-row-panel (new vertical-panel% [parent option-panel] [alignment (list 'left 'center)]))
(define top-row-panel    (new vertical-panel% [parent option-panel] [alignment (list 'center 'center)]))

(define pause-btn (new button% [parent top-row-panel]
                       [label "Play/Pause"]
                       [callback (lambda (button event) 
                                   (if (not (thread? *ga-thread*))
                                     (new-run)
                                     (set! *settings* (struct-copy settings *settings* [pause (not (settings-pause *settings*))]))))]))

(define restart-btn (new button% [parent top-row-panel]
                         [label "Restart"]
                         [callback (lambda (button event) (new-run-thread))]))

(define mutation-choice (new choice% [parent middle-row-panel]
                             [label "Mutation"]
                             [min-width 220]
                             [choices (list "Random" "Insertion" "Inversion" "Exchange" "Scramble")]
                             [callback (lambda (choice event) 
                                         (set! *settings* (struct-copy settings *settings* [mutation (send choice get-string-selection)])))]))

(define selection-choice (new choice% [parent middle-row-panel]
                              [label "Selection"]
                              [min-width 220]
                              [choices (list "Random" "Ranked" "Tournament" "Roulette")]
                              [callback (lambda (choice event)
                                          (set! *settings* (struct-copy settings *settings* [selection (send choice get-string-selection)])))]))

(define crossover-choice (new choice% [parent middle-row-panel]
                              [label "Crossover"]
                              [min-width 220]
                              [choices (list "Random" "Position Based" "Partially Mapped")]
                              [callback (lambda (choice event)
                                          (set! *settings* (struct-copy settings *settings* [crossover (send choice get-string-selection)])))]))

(define mutation-slider (new slider% [parent middle-row-panel]
                             [label "Mutation %"]
                             [min-value 0]
                             [max-value 100]
                             [callback (lambda (choice event)
                                         (set! *settings* (struct-copy settings *settings*
                                                                       [mutation-prob (/ (send choice get-value) 100)])))]))

;; Initialize the slider
(send mutation-slider set-value (inexact->exact (* 100 (settings-mutation-prob *settings*))))

(define (new-run-thread)
  (if (thread? *ga-thread*) (kill-thread *ga-thread*) null)
  (set! *ga-thread* (thread new-run)))

(define (new-run)
  (new-run-with-params *settings* (s-select *tournament-size* *prob-best* *num-best-children* *popsize*)))
;  (let* ([population (initialize-population (create-random-tour *coords*) 0 *popsize*)]
;         [xmin   (car  (argmin car  (car population)))]
;         [xmax   (car  (argmax car  (car population)))]
;         [ymin   (cadr (argmin cadr (car population)))]
;         [ymax   (cadr (argmax cadr (car population)))]
;         [strlen (length (car population))])
;    (set! *i* 0)
;    (set! *settings* (struct-copy settings *settings* [pause #f]))
;    (loop *settings*
;          (s-select *tournament-size* *prob-best* *num-best-children* *popsize*)
;          (send crossover-choice get-string-selection)
;          (send mutation-choice get-string-selection)
;          population *popsize* strlen (world null xmin xmax ymin ymax) (settings-mutation-prob *settings*))))

(define (new-run-with-params popsize algo-settings sel-settings)
  (let* ([population (initialize-population (create-random-tour *coords*) 0 popsize)]
         [xmin   (car  (argmin car  (car population)))]
         [xmax   (car  (argmax car  (car population)))]
         [ymin   (cadr (argmin cadr (car population)))]
         [ymax   (cadr (argmax cadr (car population)))]
         [strlen (length (car population))])
    (set! *i* 0)
    (set! *settings* (struct-copy settings *settings* [pause #f]))
    (loop algo-settings
          sel-settings
          population 
          popsize
          strlen
          (world null xmin xmax ymin ymax))))






;; *******
;; Drawing
;; *******
;(struct s-select (tsize bprob ranked-base popsize))
(define (loop algo-settings sel-settings oldpop popsize strlen w)
  (if (settings-pause *settings*) 
    (loop algo-settings sel-settings oldpop popsize strlen w)
    (let* ([fits  (map calc-fitness oldpop)]
           [fpop  (zip fits oldpop)]
           [best  (argmin car fpop)]
           [worst (argmax car fpop)])
      (cond 
        [(< (car best) 7550) (printf "    -> best ~a\n" (car best)) best]
        ;(update-tour-view (number->string *i*) (number->string (car best)) (number->string (car worst)) (struct-copy world w [points (cdr best)]))]
        [(> *i* 1500) best]
        [else

          ; Increment the generation counter
          (set! *i* (add1 *i*))

          ; Update the tour view
          ;(update-tour-view (number->string *i*) (number->string (car best)) (number->string (car worst)) (struct-copy world w [points (cdr best)]))

          ; Print the best
          ;(display (car best)) (display (convert-to-nums (cdr best))) (newline)

          ; Generate the next population and recurse
          (loop algo-settings sel-settings
                (append 
                  (cdr (create-new-pop algo-settings fpop (s-select-tsize sel-settings) (s-select-bprob sel-settings) (s-select-ranked-base sel-settings) popsize strlen)) 
                  (list (cdr best)))
                popsize strlen w)]))))

;(define (loop crossover mutation oldpop popsize strlen w mutation-rate)
;  (if (settings-pause *settings*) 
;    (loop crossover mutation oldpop popsize strlen w mutation-rate)
;    (let* ([fits  (map calc-fitness oldpop)]
;           [fpop  (zip fits oldpop)]
;           [best  (argmin car fpop)]
;           [worst (argmax car fpop)])
;      (cond 
;        ;[(< (car best) 7550)
;        ; (display best)
;        ; (update-tour-view (number->string *i*) (number->string (car best)) (number->string (car worst)) (struct-copy world w [points (cdr best)]))]
;        ;[(> *i* 1200) (new-run)]
;        [else
;
;          ; Increment the generation counter
;          (set! *i* (add1 *i*))
;
;          ; Update the tour view
;          (update-tour-view (number->string *i*) (number->string (car best)) (number->string (car worst)) (struct-copy world w [points (cdr best)]))
;
;          ; Print the best
;          (display (car best)) (display (convert-to-nums (cdr best))) (newline)
;
;          ; Generate the next population and recurse
;          (loop crossover mutation 
;                (append 
;                  (cdr (create-new-pop fpop *tournament-size* *prob-best* *num-best-children* popsize strlen mutation-rate)) 
;                  (list (cdr best)))
;                popsize strlen w mutation-rate)]))))

;; *****
;; Start
;; *****

;(if (not *verbose*)
;
;  ;; If verbose is turned off, start the GUI
;  (begin (start-gui) (new-run-thread))
;
;  ;; Otherwise print a bunch of example output
;  (let ([p1 ((create-random-tour *coords*) null)]
;        [p2 ((create-random-tour *coords*) null)]
;        [pop (initialize-population (create-random-tour *coords*) 0 10)])
;    (crossover-position-based   p1 p2 (length p1)) (newline)
;    (crossover-partially-mapped p1 p2 (length p1)) (newline)
;    (display "Mutation Inversion  ") (display (mutation-inversion (list 1 2 3 4 5 6 7 8 9 10) 10)) (newline)
;    (display "Mutation Scramble   ") (display (mutation-scramble  (list 1 2 3 4 5 6 7 8 9 10) 10)) (newline)
;    (display "Mutation Exchange   ") (display (mutation-exchange  (list 1 2 3 4 5 6 7 8 9 10) 10)) (newline)
;    (display "Mutation Insertion  ") (display (mutation-insertion (list 1 2 3 4 5 6 7 8 9 10) 10)) (newline)
;    (display "Ranked Selection    ") (display (convert-to-nums (cdr (selection-ranked (zip (map calc-fitness pop) pop) 
;                                                                                      (s-select 0 0 5 10))))) (newline)
;    (display "Ranked Selection    ") (display (selection-ranked
;                                                (list (list 1 1 1) (list 4 4 4) (list 5 5 5))
;                                                (s-select 0 0 1.5 3))) (newline)
;    null))

;(define *settings*          (settings #f #t "Random" "Random" "Random" 0.5))
(struct params (settings select) #:transparent)
;; Loop 1500 generations for 5 runs each
(define *param-list* (list

                       ; *0.5 mutation*
                       ;; **** Test tournament sizes ****
                       (params (settings #f #f "Random" "Random" "Tournament" 0.5) (s-select 5 0.65 2   50))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.5) (s-select 12 0.65 2  50))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.5) (s-select 20 0.65 2  50))

                       ;; **** Test tournament best probability ****
                       (params (settings #f #f "Random" "Random" "Tournament" 0.5) (s-select 5 0.5 2    50))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.5) (s-select 12 0.7 2   50))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.5) (s-select 20 0.9 2   50))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.5) (s-select 20 1 2     50))

                       ;; **** Test Ranked Selection Base ****
                       (params (settings #f #f "Random" "Random" "Ranked"     0.5) (s-select 5 0.5 1    50))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.5) (s-select 5 0.5 1.5  50))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.5) (s-select 5 0.5 2    50))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.5) (s-select 5 0.5 5    50))

                       ;; **** Test Roulette ****
                       (params (settings #f #f "Random" "Random" "Roulette"   0.5) (s-select 5 0.5 2    50))

                       ;; **** Test Random ****
                       (params (settings #f #f "Random" "Random" "Random"     0.5) (s-select 5 0.5 2    50))


                       ; *0.1 mutation*
                       ;; **** Test tournament sizes ****
                       (params (settings #f #f "Random" "Random" "Tournament" 0.1) (s-select 5 0.65 2   50))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.1) (s-select 12 0.65 2  50))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.1) (s-select 20 0.65 2  50))

                       ;; **** Test tournament best probability ****
                       (params (settings #f #f "Random" "Random" "Tournament" 0.1) (s-select 5 0.5 2    50))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.1) (s-select 12 0.7 2   50))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.1) (s-select 20 0.9 2   50))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.1) (s-select 20 1 2     50))

                       ;; **** Test Ranked Selection Base ****
                       (params (settings #f #f "Random" "Random" "Ranked"     0.1) (s-select 5 0.5 1    50))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.1) (s-select 5 0.5 1.5  50))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.1) (s-select 5 0.5 2    50))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.1) (s-select 5 0.5 5    50))

                       ;; **** Test Roulette ****
                       (params (settings #f #f "Random" "Random" "Roulette"   0.1) (s-select 5 0.5 2    50))

                       ;; **** Test Random ****
                       (params (settings #f #f "Random" "Random" "Random"     0.1) (s-select 5 0.5 2    50))


                       ; *0.05 mutation*
                       ;; **** Test tournament sizes ****
                       (params (settings #f #f "Random" "Random" "Tournament" 0.05) (s-select 5 0.65 2  50))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.05) (s-select 12 0.65 2 50))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.05) (s-select 20 0.65 2 50))

                       ;; **** Test tournament best probability ****
                       (params (settings #f #f "Random" "Random" "Tournament" 0.05) (s-select 5 0.5 2   50))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.05) (s-select 12 0.7 2  50))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.05) (s-select 20 0.9 2  50))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.05) (s-select 20 1 2    50))

                       ;; **** Test Ranked Selection Base ****
                       (params (settings #f #f "Random" "Random" "Ranked"     0.05) (s-select 5 0.5 1   50))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.05) (s-select 5 0.5 1.5 50))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.05) (s-select 5 0.5 2   50))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.05) (s-select 5 0.5 5   50))

                       ;; **** Test Roulette ****
                       (params (settings #f #f "Random" "Random" "Roulette"   0.05) (s-select 5 0.5 2   50))

                       ;; **** Test Random ****
                       (params (settings #f #f "Random" "Random" "Random"     0.05) (s-select 5 0.5 2   50))
                       

            ; 100 pop
                       
                       ; *0.5 mutation*
                       ;; **** Test tournament sizes ****
                       (params (settings #f #f "Random" "Random" "Tournament" 0.5) (s-select 5 0.65 2   100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.5) (s-select 12 0.65 2  100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.5) (s-select 20 0.65 2  100))

                       ;; **** Test tournament best probability ****
                       (params (settings #f #f "Random" "Random" "Tournament" 0.5) (s-select 5 0.5 2    100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.5) (s-select 12 0.7 2   100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.5) (s-select 20 0.9 2   100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.5) (s-select 20 1 2     100))

                       ;; **** Test Ranked Selection Base ****
                       (params (settings #f #f "Random" "Random" "Ranked"     0.5) (s-select 5 0.5 1    100))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.5) (s-select 5 0.5 1.5  100))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.5) (s-select 5 0.5 2    100))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.5) (s-select 5 0.5 5    100))

                       ;; **** Test Roulette ****
                       (params (settings #f #f "Random" "Random" "Roulette"   0.5) (s-select 5 0.5 2    100))

                       ;; **** Test Random ****
                       (params (settings #f #f "Random" "Random" "Random"     0.5) (s-select 5 0.5 2    100))


                       ; *0.1 mutation*
                       ;; **** Test tournament sizes ****
                       (params (settings #f #f "Random" "Random" "Tournament" 0.1) (s-select 5 0.65 2   100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.1) (s-select 12 0.65 2  100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.1) (s-select 20 0.65 2  100))

                       ;; **** Test tournament best probability ****
                       (params (settings #f #f "Random" "Random" "Tournament" 0.1) (s-select 5 0.5 2    100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.1) (s-select 12 0.7 2   100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.1) (s-select 20 0.9 2   100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.1) (s-select 20 1 2     100))

                       ;; **** Test Ranked Selection Base ****
                       (params (settings #f #f "Random" "Random" "Ranked"     0.1) (s-select 5 0.5 1    100))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.1) (s-select 5 0.5 1.5  100))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.1) (s-select 5 0.5 2    100))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.1) (s-select 5 0.5 5    100))

                       ;; **** Test Roulette ****
                       (params (settings #f #f "Random" "Random" "Roulette"   0.1) (s-select 5 0.5 2    100))

                       ;; **** Test Random ****
                       (params (settings #f #f "Random" "Random" "Random"     0.1) (s-select 5 0.5 2    100))


                       ; *0.05 mutation*
                       ;; **** Test tournament sizes ****
                       (params (settings #f #f "Random" "Random" "Tournament" 0.05) (s-select 5 0.65 2  100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.05) (s-select 12 0.65 2 100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.05) (s-select 20 0.65 2 100))

                       ;; **** Test tournament best probability ****
                       (params (settings #f #f "Random" "Random" "Tournament" 0.05) (s-select 5 0.5 2   100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.05) (s-select 12 0.7 2  100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.05) (s-select 20 0.9 2  100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.05) (s-select 20 1 2    100))

                       ;; **** Test Ranked Selection Base ****
                       (params (settings #f #f "Random" "Random" "Ranked"     0.05) (s-select 5 0.5 1   100))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.05) (s-select 5 0.5 1.5 100))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.05) (s-select 5 0.5 2   100))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.05) (s-select 5 0.5 5   100))

                       ;; **** Test Roulette ****
                       (params (settings #f #f "Random" "Random" "Roulette"   0.05) (s-select 5 0.5 2   100))

                       ;; **** Test Random ****
                       (params (settings #f #f "Random" "Random" "Random"     0.05) (s-select 5 0.5 2   100))
            ; 150 pop
                       ; *0.5 mutation*
                       ;; **** Test tournament sizes ****
                       (params (settings #f #f "Random" "Random" "Tournament" 0.5) (s-select 5 0.65 2   100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.5) (s-select 12 0.65 2  100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.5) (s-select 20 0.65 2  100))

                       ;; **** Test tournament best probability ****
                       (params (settings #f #f "Random" "Random" "Tournament" 0.5) (s-select 5 0.5 2    100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.5) (s-select 12 0.7 2   100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.5) (s-select 20 0.9 2   100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.5) (s-select 20 1 2     100))

                       ;; **** Test Ranked Selection Base ****
                       (params (settings #f #f "Random" "Random" "Ranked"     0.5) (s-select 5 0.5 1    100))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.5) (s-select 5 0.5 1.5  100))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.5) (s-select 5 0.5 2    100))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.5) (s-select 5 0.5 5    100))

                       ;; **** Test Roulette ****
                       (params (settings #f #f "Random" "Random" "Roulette"   0.5) (s-select 5 0.5 2    100))

                       ;; **** Test Random ****
                       (params (settings #f #f "Random" "Random" "Random"     0.5) (s-select 5 0.5 2    100))


                       ; *0.1 mutation*
                       ;; **** Test tournament sizes ****
                       (params (settings #f #f "Random" "Random" "Tournament" 0.1) (s-select 5 0.65 2   100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.1) (s-select 12 0.65 2  100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.1) (s-select 20 0.65 2  100))

                       ;; **** Test tournament best probability ****
                       (params (settings #f #f "Random" "Random" "Tournament" 0.1) (s-select 5 0.5 2    100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.1) (s-select 12 0.7 2   100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.1) (s-select 20 0.9 2   100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.1) (s-select 20 1 2     100))

                       ;; **** Test Ranked Selection Base ****
                       (params (settings #f #f "Random" "Random" "Ranked"     0.1) (s-select 5 0.5 1    100))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.1) (s-select 5 0.5 1.5  100))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.1) (s-select 5 0.5 2    100))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.1) (s-select 5 0.5 5    100))

                       ;; **** Test Roulette ****
                       (params (settings #f #f "Random" "Random" "Roulette"   0.1) (s-select 5 0.5 2    100))

                       ;; **** Test Random ****
                       (params (settings #f #f "Random" "Random" "Random"     0.1) (s-select 5 0.5 2    100))


                       ; *0.05 mutation*
                       ;; **** Test tournament sizes ****
                       (params (settings #f #f "Random" "Random" "Tournament" 0.05) (s-select 5 0.65 2  100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.05) (s-select 12 0.65 2 100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.05) (s-select 20 0.65 2 100))

                       ;; **** Test tournament best probability ****
                       (params (settings #f #f "Random" "Random" "Tournament" 0.05) (s-select 5 0.5 2   100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.05) (s-select 12 0.7 2  100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.05) (s-select 20 0.9 2  100))
                       (params (settings #f #f "Random" "Random" "Tournament" 0.05) (s-select 20 1 2    100))

                       ;; **** Test Ranked Selection Base ****
                       (params (settings #f #f "Random" "Random" "Ranked"     0.05) (s-select 5 0.5 1   100))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.05) (s-select 5 0.5 1.5 100))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.05) (s-select 5 0.5 2   100))
                       (params (settings #f #f "Random" "Random" "Ranked"     0.05) (s-select 5 0.5 5   100))

                       ;; **** Test Roulette ****
                       (params (settings #f #f "Random" "Random" "Roulette"   0.05) (s-select 5 0.5 2   100))

                       ;; **** Test Random ****
                       (params (settings #f #f "Random" "Random" "Random"     0.05) (s-select 5 0.5 2   100))
                       ))


;; For each in param-list
;;     run 5 times, keep average
;;     display params and average
(define *runs* 5)
(define *before* 0)
(define (compare params)
  (for ([p params])
       (set! *before* (current-inexact-milliseconds))
       (printf "Running with Selection(~a) Mutation(~a) TSize(~a) PBest(~a) NBest(~a) Popsize(~a)\n" 
               (settings-selection     (params-settings p))
               (settings-mutation-prob (params-settings p))
               (s-select-tsize         (params-select   p))
               (s-select-bprob         (params-select   p))
               (s-select-ranked-base   (params-select   p))
               (s-select-popsize       (params-select   p)))
       (let ([avg (/ (foldl + 0 (map (lambda (x)
                                       (car (new-run-with-params (s-select-popsize (params-select p)) (params-settings p) (params-select p)))
                                       ) (range 0 *runs*))) 
                     *runs*)])
         (printf "     -> avg ~a\n" avg)
         (printf "     -> mls ~a\n" (/ (- (current-inexact-milliseconds) *before*)))
         )))

(compare *param-list*)
