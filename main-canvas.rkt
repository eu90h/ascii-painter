#lang racket/gui

(provide main-canvas%)

(require ascii-canvas "scene.rkt" "point.rkt" "util.rkt" "camera.rkt" "history.rkt" "brush.rkt")

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

  (define/public (set-history h) (set! history h))
  (define/public (get-history) history)
  (define/public (set-cur-tile t) (set! cur-tile t))
  (define/public (set-scene s) (set! scene s))
  (define/public (set-camera c) (when (is-a? c camera%) (set! camera c)))
  (define/public (set-brush b) (set! cur-brush b))
  (define/public (set-scales x y) (set! x-scale x) (set! y-scale y))
  (define/public (set-scene-dimensions w h) (set! scene-width w) (set! scene-height h))

  (define/public (clamp mx my)
    (pt-add (pt (floor (/ mx x-scale)) (floor (/ my y-scale))) camera-pos))

  (define/override (on-char key-event)
    (case (send key-event get-key-code)
      [(menu release) (void)]
      [(escape) (let ([f (new dialog% [label "Quit?"])]) 
        (if (eq? 'yes (message-box "Exit" "Are you sure you want to exit?" f '(yes-no))) (exit) (void)) (send f show #f))]
      [(#\z)  (set! history (undo-last-action history scene)) (send this scene-draw)]
      [(up #\w) (send camera move 0 -1) (set! camera-pos (send camera get-position)) (send this scene-draw)]
      [(left #\a) (send camera move -1 0) (set! camera-pos (send camera get-position)) (send this scene-draw)]
      [(down #\s) (send camera move 0 1) (set! camera-pos (send camera get-position)) (send this scene-draw)]
      [(right #\d) (send camera move 1 0)(set! camera-pos (send camera get-position)) (send this scene-draw)])
    this)

  (define (pt-in-scene? p)
    (and (<= 0 (pt-x p)) (<= 0 (pt-y p)) (< (pt-x p) scene-width) (< (pt-y p) scene-height)))

  (define/override (on-event mouse-event)
    (let ([p (send this clamp (send mouse-event get-x) (send mouse-event get-y))])
      (when (pt-in-scene? p)
        (let ([q (pt-sub p camera-pos)]) 
          (draw-tile (send scene get (+ (pt-x camera-pos) (pt-x last-mouse-pt)) (+ (pt-y camera-pos) (pt-y last-mouse-pt)))
            (pt-x last-mouse-pt) (pt-y last-mouse-pt))
          (draw-tile cur-tile (pt-x q) (pt-y q))
          (unless (null? last-selected-points)
            (unselect-tiles last-selected-points))
          (set! last-mouse-pt q))
      (send cur-brush handle mouse-event)
      (set! history (history-add-actions history (send cur-brush get-history)))))
      (send container refresh))

  (define/public (get-width-in-chars) width)

  (define/public (scene-draw)
    (let ([cx (pt-x camera-pos)] [cy (pt-y camera-pos)])
      (for* ([xi (in-range width)] [yi (in-range height)])
        (send this draw-tile (send scene get (+ cx xi) (+ cy yi)) xi yi)))
    (send container refresh))

  (define (coords-in-canvas? x y)
    (and (>= y 0) (>= x 0) (< x width) (< y height)))

  (define/public (draw-tile tile x y)
    (when (coords-in-canvas? x y)
      (send this write (tile-symbol tile) x y (tile-fg tile) (tile-bg tile))))

  (define (unselect-tiles points)
    (set! last-selected-points null)
    (let ([cx (pt-x camera-pos)] [cy (pt-y camera-pos)] [num-pts (length points)] 
          [sym (tile-symbol selection-tile)] [fg (tile-fg selection-tile)] [bg (tile-bg selection-tile)])
      (let loop ([ps points])
        (unless (null? ps)
          (let* ([qx (- (pt-x (first ps)) cx)] [qy (- (pt-y (first ps)) cy)] [t (send scene get qx qy)])
            (when (coords-in-canvas? qx qy) (send this write (tile-symbol t) qx qy (tile-fg t) (tile-bg t))))
          (loop (rest ps))))))

  (define/public (draw-selected-tiles points)
    (set! last-selected-points points)
    (let ([cx (pt-x camera-pos)] [cy (pt-y camera-pos)] [num-pts (length points)] 
          [sym (tile-symbol selection-tile)] [fg (tile-fg selection-tile)] [bg (tile-bg selection-tile)])
      (let loop ([ps points])
        (unless (null? ps)
          (let ([qx (- (pt-x (first ps)) cx)] [qy (- (pt-y (first ps)) cy)])
            (when (coords-in-canvas? qx qy) (send this write sym qx qy fg bg)))
          (loop (rest ps))))))))

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