#lang racket

(provide update-tour-view start-gui world)

(require racket/gui/base)
(require (planet williams/animated-canvas/animated-canvas))

(define *width* 400)
(define *height* 400)

(define frame (new frame% 
                   [label "Travelling Salesperson"]
                   [width *width*]
                   [height *height*]))

(define canvas (instantiate animated-canvas% (frame)
                            [style '(border)]
                            [min-width *width*] 
                            [min-height *height*]))

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


;(define (update-canvas-tour ps)
;  (let ([ps2 ()]))
;  (draw-world canvas ps))


;(define (loop ps)
;  (let ((t (current-milliseconds)))
;    (draw-world canvas ps)
;    (sleep/yield (max 0.0 (/ (- 10.0 (- (current-milliseconds) t)) 1000.0)))
;    (loop ps)))
;
;(loop points)
;(define (loop ps)
;  (let ((t (current-milliseconds)))
;    (draw-world canvas ps)
;    (sleep/yield (max 0.0 (/ (- 10.0 (- (current-milliseconds) t)) 1000.0)))
;    (loop ps)))
;
;(loop points)

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
  (draw-world canvas (project-points world *width* *height*)))

(define (start-gui)
  (send frame show #t))
