#lang racket

(provide update-tour-view start-gui frame)
(provide (struct-out world))

(require racket/gui/base)
(require (planet williams/animated-canvas/animated-canvas))

(struct world (points xmin xmax ymin ymax))

(define *width* 400)
(define *height* 400)

;; ****************
;; Add GUI elements
;; ****************

(define frame (new frame% 
                   [label "Travelling Salesperson"]
                   [width *width*]
                   [height (+ 100 *height*)]))

(define canvas (instantiate animated-canvas% (frame)
                            [style '(border)]
                            [min-width *width*] 
                            [min-height *height*]))

;; *******
;; Drawing
;; *******

;; Pens and brushes
(define no-pen      (make-object pen%   "BLACK" 1 'transparent))
(define black-pen   (make-object pen%   "BLACK" 1 'solid))
(define no-brush    (make-object brush% "BLACK"   'transparent))
(define black-brush (make-object brush% "BLACK"   'solid))
(define red-brush   (make-object brush% "RED"     'solid))

(define (draw-tour ps)
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

(define (draw-world canvas i best-score worst-score ps)
  (let [(dc (send canvas get-dc))]
    (send dc erase)
    (send dc set-alpha 1)
    (draw-points ps dc)
    (send dc set-pen black-pen)
    (send dc set-brush no-brush)
    (send dc draw-path (draw-tour ps))
    (send dc set-alpha 0.5)
    (send dc draw-text best-score 0 0)
    (send dc draw-text worst-score 0 20)
    (send dc draw-text i 0 40))
  (send canvas swap-bitmaps))

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

(define (update-tour-view i best-score worst-score world) 
  (draw-world canvas i best-score worst-score (project-points world *width* *height*)))

(define (start-gui) (send frame show #t))
