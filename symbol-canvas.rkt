#lang racket/gui

(provide symbol-canvas%)

(require ascii-canvas "scene.rkt" "symbol.rkt" "point.rkt" "interval.rkt" "util.rkt" "history.rkt" "brush.rkt")

(define symbol-canvas% (class ascii-canvas%
  (init-field container scene set-tile-callback)
  
  (field [cur-tile empty-tile])
  (field [cur-tile-table-offset '(0 0)])

  (super-new [parent container] [width-in-characters 16] [height-in-characters 16])

  (field [x-scale (/ (send this get-width) 16)] [y-scale (/ (send this get-height) 16)])

  (field [width 16] [height 16] [x-interval (interval 0 (sub1 width))] [y-interval (interval 0 (sub1 height))]
    [data (for/vector ([x width]) 
            (for/vector ([y height]) 
              (tile (integer->symbol (modulo (+ (* 16 y) x) 255)) 
                    (make-object color% 255 255 255 1.0) 
                    (make-object color% 0 0 0 1.0)
                    (list-ref cp437-strings (modulo (+ (* 16 y) x) 255)))))])

  (define/public (set-scales x y) (set! x-scale x) (set! y-scale y))
  (define/public (get-tile) cur-tile)
  (define/public (get-table-offset) cur-tile-table-offset)
  (define/public (clamp mx my)
    (pt (floor (/ mx x-scale))
        (floor (/ my y-scale))))

  (define (process-tiles fn) (for* ([x (in-range 16)] [y (in-range 16)])
    (set x y (fn (send this symbol-table-lookup x y)))))

  (define (set x y tile) (when (good-xy? x y) (vector-set! (vector-ref data x) y tile)))

  (define/public (dye-tiles fg bg)
    (process-tiles (lambda (t) (tile (tile-symbol t) fg bg (tile-descr t))))
    (draw-symbol-table)
    (send container refresh))

  (define/override (on-event mouse-event)
    (when (eq? 'left-up (send mouse-event get-event-type))
    (let* ([p (send this clamp (send mouse-event get-x) (send mouse-event get-y))])
      (set! cur-tile-table-offset (list (pt-x p) (pt-y p)))
      (set! cur-tile (symbol-table-lookup (pt-x p) (pt-y p)))
      (set-tile-callback cur-tile cur-tile-table-offset))))

  (define/public (draw)
    (draw-symbol-table) 
    (send container refresh))

  (define (good-xy? x y) 
    (and (number-in-interval? x x-interval) (number-in-interval? y y-interval)))

  (define/public (symbol-table-lookup x y) 
    (if (good-xy? x y) 
      (vector-ref (vector-ref data x) y) 
      (vector-ref (vector-ref data 0) 0)))

  (define/public (draw-tile tile x y)
    (send this write (tile-symbol tile) x y (tile-fg tile) (tile-bg tile)))

  (define (draw-symbol-table)
    (for* ([x (in-range width)] [y (in-range height)])
      (draw-tile (symbol-table-lookup x y) x y)))))