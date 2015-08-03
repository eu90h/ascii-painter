#lang racket/gui

(provide main-canvas%)

(require ascii-canvas racket/unsafe/ops "scene.rkt" "point.rkt" "util.rkt" "camera.rkt" "history.rkt" "brush.rkt")

(define main-canvas% (class ascii-canvas%
  (init-field container width height scene camera cur-tile history)
  (super-new [parent container] [width-in-characters width] [height-in-characters height])

  (field [cur-brush null])
  (field [scene-width (send scene get-width)])
  (field [scene-height (send scene get-height)])
  (field [x-scale (/ (send this get-width) width)])
  (field [y-scale (/ (send this get-height) height)])
  (field [last-mouse-pt (pt 0 0)])
  (field [last-selected-points null])
  (field [camera-pos (send camera get-position)])
  (field [last-mouse-direction (pt 0 0)])

  (define/public (set-history h) (set! history h))
  (define/public (get-history) history)
  (define/public (set-cur-tile t) (set! cur-tile t))
  (define/public (set-scene s) (set! scene s))
  (define/public (set-camera c) (when (is-a? c camera%) (set! camera c)))
  (define/public (set-brush b) (set! cur-brush b))
  (define/public (set-scales x y) (set! x-scale x) (set! y-scale y))
  (define/public (set-scene-dimensions w h) (set! scene-width w) (set! scene-height h))

  (define/public (clamp mx my)
    (pt (unsafe-fx+ (unsafe-struct-ref camera-pos 0) 
                    (unsafe-fl->fx (unsafe-flfloor (unsafe-fx->fl (unsafe-fxquotient mx x-scale)))))
        (unsafe-fx+ (unsafe-struct-ref camera-pos 1)
                    (unsafe-fl->fx (unsafe-flfloor (unsafe-fx->fl (unsafe-fxquotient my y-scale)))))))

  (define/override (on-char key-event)
    (case (send key-event get-key-code)
      [(menu release) (void)]
      [(escape) (let ([f (new dialog% [label "Quit?"])]) 
        (if (eq? 'yes (message-box "Exit" "Are you sure you want to exit?" f '(yes-no))) (exit) (void)) (send f show #f))]
      [(#\z) (when (send key-event get-control-down) (set! history (undo-last-action history scene)) (send this scene-draw))]
      [(up #\w) (send camera move 0 -1) (set! camera-pos (send camera get-position)) (send this scene-draw)]
      [(left #\a) (send camera move -1 0) (set! camera-pos (send camera get-position)) (send this scene-draw)]
      [(down #\s) (send camera move 0 1) (set! camera-pos (send camera get-position)) (send this scene-draw)]
      [(right #\d) (send camera move 1 0)(set! camera-pos (send camera get-position)) (send this scene-draw)])
    this)

  (define (pt-in-scene? p)
    (and (unsafe-fx<= 0 (pt-x p)) (unsafe-fx<= 0 (pt-y p)) (unsafe-fx< (pt-x p) scene-width) (unsafe-fx< (pt-y p) scene-height)))

  (define/override (on-event mouse-event)
    (let ([p (send this clamp (send mouse-event get-x) (send mouse-event get-y))])
      (when (pt-in-scene? p)
        (let ([q (pt-sub p camera-pos)]) 
          (draw-tile (send scene get (unsafe-fx+ (unsafe-struct-ref camera-pos 0) (unsafe-struct-ref last-mouse-pt 0)) 
                                    (unsafe-fx+ (unsafe-struct-ref camera-pos 1) (unsafe-struct-ref last-mouse-pt 1)))
                    (unsafe-struct-ref last-mouse-pt 0) (unsafe-struct-ref last-mouse-pt 1))
          (draw-tile cur-tile (unsafe-struct-ref q 0) (unsafe-struct-ref q 1))
          (unless (null? last-selected-points) 
            (unselect-tiles last-selected-points))
          (set! last-mouse-pt q))
      (send cur-brush handle mouse-event)
      (set! history (history-add-actions history (send cur-brush get-history)))))
      (send container refresh))

  (define/public (get-width-in-chars) width)

  (define (coords-in-canvas? x y)
    (and (unsafe-fx>= y 0) (unsafe-fx>= x 0) (unsafe-fx< x width) (unsafe-fx< y height)))

  (define/public (draw-tile tile x y)
    (when (coords-in-canvas? x y)
      (send this write (unsafe-struct-ref tile 0) x y (unsafe-struct-ref tile 1) (unsafe-struct-ref tile 2))))

  (define (unselect-tiles points)
    (set! last-selected-points null)
    (let ([cx (unsafe-struct-ref camera-pos 0)] [cy (unsafe-struct-ref camera-pos 1)] [num-pts (length points)] 
          [sym (unsafe-struct-ref selection-tile 0)] [fg (unsafe-struct-ref selection-tile 1)] [bg (unsafe-struct-ref selection-tile 2)])
      (let loop ([ps points])
        (unless (null? ps)
          (let* ([qx (unsafe-fx- (unsafe-struct-ref (first ps) 0) cx)] [qy (unsafe-fx- (unsafe-struct-ref (first ps) 1) cy)] [t (send scene get qx qy)])
            (when (coords-in-canvas? qx qy) (send this write (unsafe-struct-ref t 0) qx qy (unsafe-struct-ref t 1) (unsafe-struct-ref t 2))))
          (loop (rest ps))))))

  (define/public (draw-selected-tiles points)
    (set! last-selected-points points)
    (let ([cx (unsafe-struct-ref camera-pos 0)] [cy (unsafe-struct-ref camera-pos 1)] [num-pts (length points)] 
          [sym (tile-symbol selection-tile)] [fg (tile-fg selection-tile)] [bg (tile-bg selection-tile)])
      (let loop ([ps points])
        (unless (null? ps)
          (let ([qx (unsafe-fx- (unsafe-struct-ref (first ps) 0) cx)] [qy (unsafe-fx- (unsafe-struct-ref (first ps) 1) cy)])
            (when (coords-in-canvas? qx qy) (send this write sym qx qy fg bg)))
          (loop (rest ps))))))

  (define/public (scene-draw)
    (let ([cx (unsafe-struct-ref camera-pos 0)] [cy (unsafe-struct-ref camera-pos 1)])
      (for* ([xi (in-range width)] [yi (in-range height)])
        (send this draw-tile (send scene get (unsafe-fx+ cx xi) (unsafe-fx+ cy yi)) xi yi)))
    (send container refresh))))

  

(define (color? c) (and (object? c) (is-a? c color%)))
(define (canvas? c) (and (object? c) (is-a? c canvas%)))
(define (event? e) (and (object? e) (is-a? e event%)))
(define (scene? e) (and (object? e) (is-a? e scene%)))
(define tile/c (struct/c tile char? color? color? string?))
(define pt/c (struct/c pt integer? integer?))
(define/contract main-canvas+c%
  (class/c [on-event (->m event? void?)]
    [clamp (->m natural-number/c natural-number/c pt/c)]
    [scene-draw (->m void?)]
    [draw-selected-tiles (->m list? void?)]
    [draw-tile (->m tile/c natural-number/c natural-number/c void?)])
  main-canvas%)