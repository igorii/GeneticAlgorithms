#lang racket

(require racket/match)
(require racket/gui)
(require "util.rkt" "general-ga.rkt" "window.rkt")

;; **********
;; Structures
;; **********

;; Command line arguments
(struct args (file sep col) #:transparent)

;; Genetic Algorithm settings
(struct settings (mutation crossover selection mutation-prob) #:transparent)

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

;; Whether to start a GA run, or print sample output of crossovers and mutation
(define *verbose* #f)

;; Whether the GA thread is paused or not
(define *pause* #f)

;; A handle to the thread responsible for looping and rendering the genetic algorithm
(define *ga-thread* null)

;; Settings for the current run. This is updated by the GUI layer with the desired choices
(define *gui-settings* (settings "Random" "Random" "Random" 0.5))

;; Coordinate system identifiers
(define *coords* (get-coords-from-file (args-file cargs) (args-sep cargs) (args-col cargs)))
;(define *coords* (map (lambda (x) (list (* 1000 (random)) (* 1000 (random)))) (range 0 30)))

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
(define (get-range m)
  (let* ([r1           (random m)]
         [r2           (random m)]
         [point-a      (if (> r1 r2) r2 r1)]
         [point-b      (if (> r1 r2) r1 r2)])
    (list point-a point-b)))
(define (callback i best worst w)
  (update-tour-view (number->string i) (number->string (car best)) (number->string (car worst)) (struct-copy world w [points (cdr best)])))

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
         [unused  (get-unused p2 (cadr post-p1))]
         [child   (fill-nulls unused (car post-p1) '())])

    (if *verbose* 
      (begin
        (displayln "Position Based Crossover")
        (display "    Parent 1: ") (displayln (convert-to-nums p1))
        (display "    Parent 2: ") (displayln (convert-to-nums p2))
        (display "    From p1 : ") (displayln (map (lambda (x) (if (null? x) #\_ x)) (convert-to-nums (car post-p1))))
        (display "    Child   : ") (displayln (convert-to-nums child)))
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
    ;; Take the points from p1 that are within the given range
    (define (loop pos tour p1 p2 used-map)
      (cond [(null? p1) (list tour used-map)]
            [(> pos b)  (loop (add1 pos) (append tour (list null)) (cdr p1) (cdr p2) used-map)]
            [(< pos a)  (loop (add1 pos) (append tour (list null)) (cdr p1) (cdr p2) used-map)]
            [else       (loop (add1 pos) (append tour (list (car p1))) (cdr p1) (cdr p2) (hash-set used-map (car p1) #t))]))
    (loop 0 tour p1 p2 used-map))

  (define (phase2 p1 p2 a b tour used-map)
    ;; Move the points from p2 down in order (if they aren't already added)
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
         [child        (fill-nulls (get-unused p2 (cadr post-phase2)) (car post-phase2) '())])

    (if *verbose* 
      (begin
        (displayln "Partially Mapped Crossover")
        (display "    Parent 1: ") (displayln (add-points-at-indicies (convert-to-nums p1) (list #\| #\|) points))
        (display "    Parent 2: ") (displayln (add-points-at-indicies (convert-to-nums p2) (list #\| #\|) points))
        (display "    Points  : ") (displayln points)
        (display "    Part 1  : ") (displayln (add-points-at-indicies (map (lambda (x) (if (null? x) #\_ x)) (convert-to-nums (car post-phase1))) (list #\| #\|) points))
        (display "    Part 2  : ") (displayln (add-points-at-indicies (map (lambda (x) (if (null? x) #\_ x)) (convert-to-nums (car post-phase2))) (list #\| #\|) points))
        (display "    Child   : ") (displayln (add-points-at-indicies (convert-to-nums child) (list #\| #\|) points)))
      null)

    child))

;; **********
;; Selections
;; **********

;; Return the selection function with the given name
(define (lbl->selection lbl)
  (cond [(eq? lbl "Random")     (random-selection)]
        [(eq? lbl "Tournament") selection-tournament]
        [(eq? lbl "Roulette")   selection-roulette]
        [(eq? lbl "Ranked")     selection-ranked]))

;; Return a random selection function
(define (random-selection)
  (let* ([ss (vector selection-tournament selection-ranked)]
         [r  (random (vector-length ss))])
    (vector-ref ss r)))

;; Perform a tournament selection on the population. Ignore the last param (ranked)
;; since it is unused here.
(define (selection-tournament fpop select-settings _)
  (let* ([tpop (take (shuffle fpop) (s-select-tsize select-settings))])   ; Select the individuals for tournament
    (if (chance (s-select-bprob select-settings))
      (first (sort tpop (lambda (x y) (< (car x) (car y)))))              ; bprob % of the time take the best
      (first (shuffle tpop)))))                                           ; Otherwise take a random one

;; Rank the population based on a scheme presented in the course notes. This
;; avoids "super-individuals" since selection is based on reletive rank rather
;; than absolute fitness.
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
         [probs  (map assign (reverse (range 0 u)))]              ; Assign rank based probabilities
                                                                  ; Reverse the probabilities since we are minimizng the scores
         [sprobs (scan + 0 probs)])                               ; Scan addition over the probabilities
    (if *verbose* (printf "\n     Probs: ~a\n     Total: ~a\n" probs (car (reverse sprobs))) null)
    (list sorted sprobs)))

;; Perform ranked selection on a population. Rank probabilities have already
;; been assigned at this point and are provided as the third param. This is 
;; done ahead of time to avoid computing the ranks *per selection*, since the
;; ranks will not change in a single generation.
(define (selection-ranked fpop select-settings ranked)
  ;; Linear search the population while the generated number is 
  ;; greater than the current assigned probability
  (define (select r probs fpop)
    (if (< r (car probs))
      (car fpop)
      (select r (cdr probs) (cdr fpop))))

  (let* ([r        (random)]                               ; Choose a random individual
         [selected (select r (cadr ranked) (car ranked))]) ; Determine which individual was chosen
    (if *verbose* (printf "     R: ~a\n" r) null)
    selected))

;; Determine the total fitness of the population
(define (total-fitness fs) (foldl + 0 fs))

;; Perform a fitness-proportional selection on the population
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

;; Mutate an individual by moving a city to another position within
;; the tour
(define (mutation-insertion individual strlen)
  (let* ([a (random strlen)]
         [b (random strlen)]
         [remd (append (take individual a) (drop individual (add1 a)))]
         [v (car (drop individual a))])
    (if *verbose* (printf "~a ~a (~a, ~a)  " remd v a b) null)
    (insert-at remd b v)))

;; **************
;; Initialization
;; **************

;; Create a random tour simpyl by shuffling the possible cities
(define (create-random-tour domain)
  (lambda (_) (shuffle domain)))

;; *****************
;; Genetic Algorithm
;; *****************

(define (run-ga2
          #:population-size popsize
          #:domain          domain
          #:crossover       [crossover "Random"]
          #:mutation        [mutation  "Random"]
          #:selection       [selection "Random"]
          #:mutation-prob   [mutation-prob 0.5]
          #:cutoff          [cutoff          0]
          #:max-gen         [max-gen         0]
          #:tourny-size     [tsize          12]
          #:best-prob       [bprob        0.65]
          #:ranked-base     [ranked-base     2]
          #:callback        [callback        (lambda (i best worst world) null)])

  ;; Convenience for passing to selection functions
  (define select-settings (s-select tsize bprob ranked-base popsize))

  ;; Store the length for convenience
  (define strlen (length domain))

  ;; Select two individuals from the given population with replacement and return
  ;; a child of the two selected parents. The child will be mutated with probability
  ;; specified by `mutation-rate`
  (define (tsp-crossover fpop ranked)
    (let* ([sfn       (lbl->selection    selection)]
           [cfn       (lbl->crossover-fn crossover)]
           [mfn       (lbl->mutation-fn  mutation )]
           [p1        (sfn fpop select-settings ranked)]
           [p2        (sfn fpop select-settings ranked)]
           [candidate (cfn (cdr p1) (cdr p2) strlen)])
      (if (chance mutation-prob)
        (mfn candidate strlen)
        candidate)))

  ;; Create a new population by creating popsize many new tours
  ;; New tours are added to a hash map to avoid introducing duplicates
  (define (create-new-pop fpop)
    ;; Sort and rank before crossover to avoid sorting on each selection worst case
    (let ([ranked (rank-pop (s-select tsize bprob ranked-base popsize) fpop popsize)])
      (map (lambda (_) (tsp-crossover fpop ranked))
           (range 0 popsize))))

  ;; Elitism ensures that the best genetic material is not lost when swapping
  ;; generations. Add the current best to the next generation.
  (define (elitism pop best)
    (cons best (cdr pop)))

  ;(struct s-select (tsize bprob ranked-base popsize))
  (define (loop i oldpop strlen w)
    (if *pause*
      (loop i oldpop strlen w)
      (let* ([fits    (map calc-fitness oldpop)]
             [fpop    (zip fits oldpop)]
             [best    (argmin car fpop)]
             [worst   (argmax car fpop)])
        (cond [(<= (car best) cutoff)
               (callback i best worst w) best]
              [(and (not (eq? max-gen 0)) (>= i max-gen))
               (callback i best worst w) best]
              [else 
                (callback i best worst w)
                (loop (add1 i) (elitism (create-new-pop fpop) (cdr best)) strlen w)]))))

  ;; Initialize the population and set up the canvas dimensions
  ;; Then kick off the GA
  (let* ([population (initialize-population (create-random-tour domain) 0 popsize)]
         [xmin   (car  (argmin car  (car population)))]
         [xmax   (car  (argmax car  (car population)))]
         [ymin   (cadr (argmin cadr (car population)))]
         [ymax   (cadr (argmax cadr (car population)))]
         [strlen (length (car population))])
    (set! *pause* #f)
    (loop 0 population strlen (world null xmin xmax ymin ymax))))

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
                                     (set! *pause* (not *pause*))))]))

(define restart-btn (new button% [parent top-row-panel]
                         [label "Restart"]
                         [callback (lambda (button event) (new-run-thread))]))

(define mutation-choice (new choice% [parent middle-row-panel]
                             [label "Mutation"]
                             [min-width 220]
                             [choices (list "Random" "Insertion" "Inversion" "Exchange" "Scramble")]
                             [callback (lambda (choice event) 
                                         (set! *gui-settings* (struct-copy settings *gui-settings* [mutation (send choice get-string-selection)])))]))

(define selection-choice (new choice% [parent middle-row-panel]
                              [label "Selection"]
                              [min-width 220]
                              [choices (list "Random" "Ranked" "Tournament" "Roulette")]
                              [callback (lambda (choice event)
                                          (set! *gui-settings* (struct-copy settings *gui-settings* [selection (send choice get-string-selection)])))]))

(define crossover-choice (new choice% [parent middle-row-panel]
                              [label "Crossover"]
                              [min-width 220]
                              [choices (list "Random" "Position Based" "Partially Mapped")]
                              [callback (lambda (choice event)
                                          (set! *gui-settings* (struct-copy settings *gui-settings* [crossover (send choice get-string-selection)])))]))

(define mutation-slider (new slider% [parent middle-row-panel]
                             [label "Mutation %"]
                             [min-value 0]
                             [max-value 100]
                             [callback (lambda (choice event)
                                         (set! *gui-settings* (struct-copy settings *gui-settings*
                                                                           [mutation-prob (/ (send choice get-value) 100)])))]))

(send mutation-slider set-value (inexact->exact (* 100 (settings-mutation-prob *gui-settings*))))

(define tourny-size-field (new text-field% [label "Tournament size"]
                               [init-value "12"]
                               [parent middle-row-panel]))
(define tourny-best-field (new text-field% [label "Tournament % best"]
                               [init-value "0.65"]
                               [parent middle-row-panel]))
(define ranked-base-field (new text-field% [label "Ranked base"]
                               [init-value "2"]
                               [parent middle-row-panel]))
(define max-gen-field     (new text-field% [label "Max generations"]
                               [init-value "1500"]
                               [parent middle-row-panel]))
(define best-score-field  (new text-field% [label "Cutoff score"]
                               [init-value "0"]
                               [parent middle-row-panel]))
(define pop-size-field  (new text-field% [label "Population size"]
                             [init-value "50"]
                             [parent middle-row-panel]))

;; Start a GA in the GA thread, leaving the GUI thread open to receive and handle GUI events
(define (new-run-thread)
  (if (thread? *ga-thread*) (kill-thread *ga-thread*) null)
  (set! *ga-thread* (thread new-run)))

;; Initiate a new GA run n the current thread. The GA is initialized with the settings
;; appearing in the GUI. The GA will loop until the cutoff is observed.
(define (new-run)
  (let* ([cutoff (string->number (send best-score-field get-value))]
         [best (run-ga2 #:population-size (string->number (send pop-size-field get-value))
                        #:domain          *coords*
                        #:crossover       (settings-crossover      *gui-settings*)
                        #:selection       (settings-selection      *gui-settings*)
                        #:mutation        (settings-mutation       *gui-settings*)
                        #:mutation-prob   (settings-mutation-prob  *gui-settings*)
                        #:cutoff          cutoff
                        #:max-gen         (string->number (send max-gen-field get-value))
                        #:tourny-size     (string->number (send tourny-size-field get-value))
                        #:best-prob       (string->number (send tourny-best-field get-value))
                        #:ranked-base     (string->number (send ranked-base-field get-value))
                        #:callback        callback)])
    (if (< cutoff (car best))
      (new-run)
      null)))


;; *****
;; Start
;; *****

(if (not *verbose*)

  ;; If verbose is turned off, start the GUI
  (begin (start-gui) (new-run-thread))

  ;; Otherwise print a bunch of example output
  (let* ([p1 ((create-random-tour *coords*) null)]
         [p2 ((create-random-tour *coords*) null)]
         [pop (initialize-population (create-random-tour *coords*) 0 10)])
    (newline)
    (crossover-position-based   p1 p2 (length p1)) (newline)
    (crossover-partially-mapped p1 p2 (length p1)) (newline)
    (display "Mutation Inversion  ") (display (mutation-inversion (list 1 2 3 4 5 6 7 8 9 10) 10)) (newline)
    (display "Mutation Scramble   ") (display (mutation-scramble  (list 1 2 3 4 5 6 7 8 9 10) 10)) (newline)
    (display "Mutation Exchange   ") (display (mutation-exchange  (list 1 2 3 4 5 6 7 8 9 10) 10)) (newline)
    (display "Mutation Insertion  ") (display (mutation-insertion (list 1 2 3 4 5 6 7 8 9 10) 10)) (newline)

    ;; Ranked Selection examples from notes
    (newline)
    (display "Ranked Selection (s=1.5)")
    (let* ([fpop     (list (list 1 1 1) (list 4 4 4) (list 5 5 5))]
           [settings (s-select 0 0 1.5 3)]
           [ranked   (rank-pop settings fpop 3)])
      (printf "     Result: ~a\n" (selection-ranked fpop settings ranked)))

    (newline)
    (display "Ranked Selection (s=2)")
    (let* ([fpop     (list (list 1 1 1) (list 4 4 4) (list 5 5 5))]
           [settings (s-select 0 0 2 3)]
           [ranked   (rank-pop settings fpop 3)])
      (printf "     Result: ~a\n" (selection-ranked fpop settings ranked)))

    null))

