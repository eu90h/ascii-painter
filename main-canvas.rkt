#lang racket/gui

(provide main-canvas%)

(require ascii-canvas "scene.rkt" "point.rkt" "interval.rkt" "util.rkt" "camera.rkt" "history.rkt" "brush.rkt")

(define main-canvas% (class ascii-canvas%
  (init-field container width height scene camera cur-tile history)
  (super-new [parent container] [width-in-characters width] [height-in-characters height])

  (field [x-interval (interval 0 (sub1 width))] [y-interval (interval 0 (sub1 height))])
  (field [cur-brush null])
  (field [scene-x-interval (interval 0 (sub1 (send scene get-width)))])
  (field [scene-y-interval (interval 0 (sub1 (send scene get-height)))])
  (field [x-scale (/ (send this get-width) width)])
  (field [y-scale (/ (send this get-height) height)])
  (field [last-mouse-pt (pt 0 0)])

  (define/public (set-history h) (set! history h))
  (define/public (get-history) history)
  (define/public (set-cur-tile t) (set! cur-tile t))
  (define/public (set-scene s) (set! scene s))
  (define/public (set-camera c) (when (is-a? c camera%) (set! camera c)))
  (define/public (set-brush b) (set! cur-brush b))
  (define/public (set-scales x y) (set! x-scale x) (set! y-scale y))
  (define/public (set-canvas-boundary x y) (set! x-interval x) (set! y-interval y))
  (define/public (set-scene-intervals x y) (set! scene-x-interval x) (set! scene-y-interval y))

  (define/public (clamp mx my)
    (let ([camera-pos (send camera get-position)])
      (pt (+ (pt-x camera-pos) (floor (/ mx x-scale)))
          (+ (pt-y camera-pos) (floor (/ my y-scale))))))

  (define/override (on-char key-event)
    (case (send key-event get-key-code)
      [(menu release) (void)]
      [(escape) (let ([f (new dialog% [label "Quit?"])]) 
        (if (eq? 'yes (message-box "Exit" "Are you sure you want to exit?" f '(yes-no))) (exit) (void)) (send f show #f))]
      [(#\z)  (set! history (undo-last-action history scene)) (send this draw)]
      [(up #\w) (send camera move 0 -1) (send this draw)]
      [(left #\a) (send camera move -1 0) (send this draw)]
      [(down #\s) (send camera move 0 1) (send this draw)]
      [(right #\d) (send camera move 1 0) (send this draw)])
    this)

  (define/override (on-event mouse-event)
    (let ([p (send this clamp (send mouse-event get-x) (send mouse-event get-y))])
      (when (pt-within-bounds? p scene-x-interval scene-y-interval)
        (let* ([camera-pos (send camera get-position)] [q (pt-sub p camera-pos)]) 
          (draw-tile cur-tile (pt-x last-mouse-pt) (pt-y last-mouse-pt))
          (send container refresh)
          (set! last-mouse-pt q)))
      (send cur-brush handle mouse-event) 
      (set! history (history-add-actions history (send cur-brush get-history)))
      (send this draw)))

  (define/public (get-width-in-chars) width)

(define (scene-draw)
  (let ([camera-pos (send camera get-position)])
  (for* ([xi (in-range width)] [yi (in-range height)])
    (send this draw-tile (send scene get (+ (pt-x camera-pos) xi) (+ (pt-y camera-pos) yi))  xi  yi))))

  (define (good-xy? x y) 
    (and (number-in-interval? x x-interval) (number-in-interval? y y-interval)))

  (define/public (draw-tile tile canvas-x canvas-y)
    (when (good-xy? canvas-x canvas-y)
      (send this write (tile-symbol tile) canvas-x canvas-y (tile-fg tile) (tile-bg tile))))

  (define/public (draw-selected-tiles t)
    (define selected-points t)
    (define camera-pos (send camera get-position))
    (for ([i (in-range (length selected-points))])
      (define p (list-ref selected-points i))
      (define t (send scene get (pt-x p) (pt-y p)))
      (send this draw-tile (tile (tile-symbol t) (make-object color% 255 255 0 1.0) (tile-bg t) (tile-descr t))
        (- (pt-x p) (pt-x camera-pos))
        (- (pt-y p) (pt-y camera-pos)))))

  (define/public (draw) 
    (scene-draw)
    (draw-tile cur-tile (pt-x last-mouse-pt) (pt-y last-mouse-pt))
    (when (is-a? cur-brush brush-with-selection-interface) 
       (draw-selected-tiles (send cur-brush get-selected-points)))
    (send container refresh))))
