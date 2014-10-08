#lang racket

(provide update-tour-view start-gui world)

(require racket/gui/base)
(require (planet williams/animated-canvas/animated-canvas))

(define *width* 400)
(define *height* 400)

(define frame (new frame% 
                   [label "Travelling Salesperson"]
                   [width *width*]
                   [height (+ 100 *height*)]))



(define pause #f)

;; ****************
;; Add GUI elements
;; ****************

(define canvas (instantiate animated-canvas% (frame)
                            [style '(border)]
                            [min-width *width*] 
                            [min-height *height*]))

(define top-row-panel    (new horizontal-panel% [parent frame] [alignment (list 'center 'center)]))
(define middle-row-panel (new horizontal-panel% [parent frame] [alignment (list 'center 'center)]))


(new button% [parent top-row-panel]
             [label "Play/Pause"]
             [callback (lambda (button event) (set! pause (not pause)))])

(new button% [parent top-row-panel]
             [label "Restart"]
             [callback (lambda (button event) (set! pause (not pause)))])

(new choice% [parent middle-row-panel]
             [label "Mutation"]
             [choices (list "Random" "Insertion" "Inversion" "Exchange" "Scramble")]
             [callback (lambda (choice event) (display (send choice get-string-selection)) (newline))])

(new choice% [parent middle-row-panel]
             [label "Crossover"]
             [choices (list "Random" "Ranked" "Tournament")]
             [callback (lambda (choice event) (display (send choice get-string-selection)) (newline))])


;; Pens and brushes
(define no-pen      (make-object pen%   "BLACK" 1 'transparent))
(define black-pen   (make-object pen%   "BLACK" 1 'solid))
(define no-brush    (make-object brush% "BLACK"   'transparent))
(define black-brush (make-object brush% "BLACK"   'solid))
(define red-brush   (make-object brush% "RED"     'solid))

;; draw paths
(define (draw-tour ps) ; left side of the lambda
  (let ([p     (new dc-path%)]
        [first (car ps)]
        [rest  (cdr ps)])
    (send p move-to (car first) (cadr first))
    (for ([cp rest]) (send p line-to (car cp) (cadr cp)))
    (send p line-to (car first) (cadr first))
    p))

(define (draw-points ps dc)
  (send dc set-pen no-pen)
  (send dc set-brush red-brush)
  (for ([p ps])
       (send dc draw-ellipse (- (car p) 2) (- (cadr p) 2) 4 4)))

;; Canvas draw
(define (draw-world canvas ps)
  (let [(dc (send canvas get-dc))]
    (draw-points ps dc)
    (send dc set-pen black-pen)
    (send dc set-brush no-brush)
    (send dc draw-path (draw-tour ps)))
  (send canvas swap-bitmaps))


;; (: coord->point ( Number Number Number Number Number Number -> (Listof Number) ))
(define (coord->point xmin ymin xrange yrange dxrange dyrange)
  (lambda (coord)
    (let* ([x      (car coord)]
           [y      (cadr coord)]
           [xnumer (- x xmin)]
           [ynumer (- y ymin)]
           [xinter (/ xnumer xrange)]
           [yinter (/ ynumer yrange)])
      (list (* dxrange xinter) (- dyrange (* dyrange yinter))))))

(define (project-points w width height)
  (map (coord->point
         (world-xmin w)
         (world-ymin w)
         (- (world-xmax w) (world-xmin w))
         (- (world-ymax w) (world-ymin w))
         width
         height)
       (world-points w)))

(struct world (points xmin xmax ymin ymax))

(define (update-tour-view world) 
  (if pause null
  (draw-world canvas (project-points world *width* *height*))))

(define (start-gui)
  (send frame show #t))
