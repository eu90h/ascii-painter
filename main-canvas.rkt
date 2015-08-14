#lang racket/gui

(provide main-canvas%)

(require "ascii-canvas.rkt" racket/unsafe/ops "scene.rkt" "point.rkt" "util.rkt" "camera.rkt" "history.rkt" "brush.rkt")

(define unsafe-fxfloor (compose unsafe-fl->fx unsafe-flfloor unsafe-fx->fl))

(define main-canvas% (class ascii-canvas%
                       (init-field
                        container
                        width
                        height
                        scene
                        camera
                        cur-tile
                        history
                        save-tile
                        set-tile-callback)
                       
                       (super-new [parent container]
                                  [width-in-characters width]
                                  [height-in-characters height])
                       
                       (field [cur-brush null])
                       (field [scene-width (send scene get-width)])
                       (field [scene-height (send scene get-height)])
                       (field [x-scale (/ (send this get-width) width)])
                       (field [y-scale (/ (send this get-height) height)])
                       (field [last-mouse-pt (pt 0 0)])
                       (field [last-selected-points null])
                       (field [camera-pos (send camera get-position)])
                       (field [update-camera? #f])
                       (field [last-thread null])

                       (define/public (set-history h) (set! history h))
                       (define/public (get-history) history)
                       (define/public (set-cur-tile t) (set! cur-tile t))
                       (define/public (set-scene s) (set! scene s))
                       (define/public (set-camera c) (when (is-a? c camera%) (set! camera c)))
                       (define/public (set-brush b) (set! cur-brush b))
                       (define/public (set-scales x y) (set! x-scale x) (set! y-scale y))
                       (define/public (set-scene-dimensions w h) (set! scene-width w) (set! scene-height h))

                       (define/public (clamp mx my)
                         (pt (unsafe-fx+ (unsafe-pt-x camera-pos) 
                                         (unsafe-fxfloor (unsafe-fxquotient mx x-scale)))
                             (unsafe-fx+ (unsafe-pt-y camera-pos)
                                         (unsafe-fxfloor (unsafe-fxquotient my y-scale)))))

                       (define/override (on-char key-event)
                         (unless (null? last-thread)
                                        (kill-thread last-thread))
                         (case (send key-event get-key-code)
                           [(menu) (void)]
                           [(release) (set! camera-pos  (send camera get-position)) (send this scene-draw)]
                           [(escape) (let ([f (new dialog% [label "Quit?"])]) 
                                       (if (eq? 'yes (message-box "Exit" "Are you sure you want to exit?" f '(yes-no))) (exit) (void)) (send f show #f))]
                           [(#\z) (when (send key-event get-control-down) (set! history (undo-last-action history scene)) (send this scene-draw))]
                           ; this is so awful and hacky
                           ; it was just so hard to get the scrolling to be responsive
                           [(up #\w) (set! last-thread
                                           (thread (thunk
                                                    (send this scene-draw (send camera move 0 -1)))))]
                           [(left #\a)  (set! last-thread
                                           (thread (thunk
                                                    (send this scene-draw (send camera move -1 0)))))]
                           [(down) (set! last-thread
                                         (thread (thunk
                                                  (send this scene-draw (send camera move 0 1)))))]
                           [(#\s) (if (send key-event get-control-down)
                                           (save-tile container key-event)
                                           (set! last-thread
                                                 (thread (thunk
                                                          (send this scene-draw (send camera move 0 1))))))]
                           [(right #\d) (set! last-thread
                                           (thread (thunk
                                                    (send this scene-draw (send camera move 1 0)))))])
                         this)
                       
                       (define (pt-in-scene? p)
                         (and (unsafe-fx<= 0 (unsafe-pt-x p))
                              (unsafe-fx<= 0 (unsafe-pt-y p)) 
                              (unsafe-fx< (unsafe-pt-x p) scene-width)
                              (unsafe-fx< (unsafe-pt-y p) scene-height)))
                       
                       (define/override (on-event mouse-event)
                         (let ([p (send this clamp (send mouse-event get-x) (send mouse-event get-y))])
                           (when (pt-in-scene? p)
                             
                             (let ([q (pt-sub p camera-pos)])
                               (unless (equal? (send cur-brush get-name) "Select")
                                 (draw-tile (send scene get (unsafe-fx+ (unsafe-pt-x camera-pos) (unsafe-pt-x last-mouse-pt)) 
                                                  (unsafe-fx+ (unsafe-pt-y camera-pos) (unsafe-pt-y last-mouse-pt)))
                                            (unsafe-pt-x last-mouse-pt) (unsafe-pt-y last-mouse-pt))
                                 (draw-tile cur-tile (unsafe-pt-x q) (unsafe-pt-y q))
                                 (set! last-mouse-pt q)))
                             (if (equal? (send cur-brush get-name) "Select")
                                 (when (eq? (send mouse-event get-event-type) 'left-up)
                                   (let* ([m (send this clamp (send mouse-event get-x) (send mouse-event get-y))]
                                          [mx (unsafe-pt-x m)]
                                         [my (unsafe-pt-y m)])
                                       (set-tile-callback (send scene get mx my))))
                                 (send cur-brush handle mouse-event))
                             (set! history (history-add-actions history (send cur-brush get-history)))))
                         (send container refresh))
                       
                       (define/public (get-width-in-chars) width)
                       
                       (define (coords-in-canvas? x y)
                         (and (unsafe-fx>= y 0) (unsafe-fx>= x 0) (unsafe-fx< x width) (unsafe-fx< y height)))

                       (define/public (draw-tile tile x y [subtract-camera? #f])
                         (when (coords-in-canvas? x y)
                           (if subtract-camera? (send this write-tile
                                                      (unsafe-fx- x (unsafe-pt-x camera-pos))
                                                      (unsafe-fx- y (unsafe-pt-y camera-pos))
                                                      tile)
                           (send this write-tile x y tile))))
                       
                     (define/public (unselect-all)
                      (send this scene-draw))
                       
                     (define/public (select x y)
                       (let* ([cx (unsafe-pt-x camera-pos)]
                              [cy (unsafe-pt-y camera-pos)]
                              [qx (unsafe-fx- x cx)]
                              [qy (unsafe-fx- y cy)])
                         (when (coords-in-canvas? qx qy)
                           (send this write-tile
                                 qx
                                 qy
                                 selection-tile))))
                       
                       (define/public (scene-draw [camera-pos camera-pos])
                         (set! camera-pos (send camera get-position))
                         (send this for-each-tile scene (unsafe-pt-x camera-pos) (unsafe-pt-y camera-pos))
                         (send container refresh))))