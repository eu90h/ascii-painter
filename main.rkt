#lang racket/gui

(require racket/serialize ascii-canvas "scene.rkt" "symbol.rkt" "brush.rkt" "point.rkt" "util.rkt" "generator.rkt")

(define creator-fg-color (make-object color% 100 100 100))
(define creator-bg-color (make-object color% 100 100 100))

(define camera-pos (pt 0 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tiles (list empty-tile))

(define cur-tile (first tiles))
(define (set-cur-tile t)
  (set! cur-tile t)
  (send cur-brush set-tile t)
  (send fg-canvas redraw)
  (send bg-canvas redraw))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define canvas-width 70)
(define canvas-height 30)

(define scene (new scene% [width 70] [height 30] [tile empty-tile]))

(define (draw-tile canvas tile canvas-x canvas-y)  
  (send canvas write (tile-symbol tile) canvas-x canvas-y (tile-fg tile) (tile-bg tile)))

(define (scene-draw canvas scene) (for* ([xi (in-range (sub1 canvas-width))] [yi (in-range (sub1 canvas-height))])
                                    (draw-tile canvas (send scene get (+ (pt-x camera-pos) xi) (+ (pt-y camera-pos) yi))  xi  yi)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define frame (new frame% [label "Scene Editor"] [style '(no-resize-border)]))
(define brush-hpanel (new horizontal-panel% [parent frame]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define menu-bar (new menu-bar% (parent frame)))
(define file-menu (new menu% (label "&File") (parent menu-bar)))
(define generator-menu (new menu% (label "&Generators") (parent menu-bar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-callback menu evt) (change-scene (new scene% [width canvas-width] [height canvas-height] [tile empty-tile])))

(define new-menu (new menu-item% (label "New") (parent file-menu) (callback new-callback)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (save-callback menu evt) 
  (define out (open-output-file (put-file) #:mode 'binary #:exists 'replace))
	(write (serialize scene) out)
	(close-output-port out))

(define save-menu (new menu-item% (label "Save") (parent file-menu) (callback save-callback)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (load-callback menu evt)
  (define in (open-input-file (get-file) #:mode 'binary))
  (set! scene (deserialize (read in)))
  
  (define loaded-tiles (remove-duplicates (flatten (map vector->list (vector->list (send scene get-data))))))
  (for ([i (in-range (length loaded-tiles))])
    (define t (list-ref loaded-tiles i))
    (set! tiles (append tiles (list t)))
    (send tile-choices append (tile-descr t)))
  (close-input-port in)
  (send canvas draw))

(define load-menu (new menu-item% (label "Load") (parent file-menu) (callback load-callback)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define canvas (new (class ascii-canvas%
                      (super-new [parent frame] [width-in-characters canvas-width] [height-in-characters canvas-height])
                      
                      (field [x-scale (/ (send this get-width) canvas-width)])
                      (field [y-scale (/ (send this get-height) canvas-height)])
                      (define/public (set-scales x y) (set! x-scale x) (set! y-scale y))
                      (define/public (clamp mx my)
                        (pt (+ (pt-x camera-pos) (round (/ mx x-scale)))
                            (+ (pt-y camera-pos) (round (/ my y-scale)))))
                         (define (too-low? x y) (or (< x 0) (< y 0)))
                        (define (too-high? x y) (or (>= x (sub1 (send scene get-width))) (>= y (sub1 (send scene get-height)))))
                      (define (valid-pos? p)
                     
                         (and (false? (too-low? (pt-x p) (pt-y p))) (false? (too-high? (pt-x p) (pt-y p)))))
                      (define (safe-add p)
                        (if (valid-pos? (pt-add camera-pos p)) (pt-add camera-pos p) camera-pos))
                      (define/override (on-char key-event)
                        (case (send key-event get-key-code)
                          [(escape) (exit)]
                          [(release menu) (void)]
                          [(up #\w) (set! camera-pos (safe-add  (pt 0 -1))) (send this draw)]
                          [(left #\a) (set! camera-pos (safe-add  (pt -1 0))) (send this draw)]
                          [(down #\s) (set! camera-pos (safe-add  (pt 0 1))) (send this draw)]
                          [(right #\d) (set! camera-pos (safe-add  (pt 1 0))) (send this draw)])
                        this)
                      
                      (define/override (on-event mouse-event) 
                        (send cur-brush handle mouse-event)
                        (when (eq? cur-brush selection-brush)
                          (update-info-panel)))
                      
                      (define/public (get-width-in-chars) canvas-width)
                      
                      (define/public (draw-selected-tiles)
                        (define selected-points (send cur-brush get-selected-points))
                        (define selected-tiles (send cur-brush get-selected-tiles))
                        
                        (for ([i (in-range (length selected-tiles))])
                          (define t (list-ref selected-tiles i))
                          (define p (list-ref selected-points i))
                          (draw-tile canvas (tile (tile-symbol t) (tile-bg t) (tile-fg t) (tile-descr t)) (+ (pt-x camera-pos) (pt-x p))
                                     (+ (pt-y camera-pos) (pt-y p)))))
                      
                      (define/public (draw) (scene-draw this scene) (send frame refresh)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (change-scene s)
  (set! scene s)
  (map (lambda (b) (send b set-scene s)) brushes)
  (send canvas draw))

(define (fill-generator-callback menu evt)
  (let ([fill-generator (make-object fill-generator% scene canvas tiles)])
    (send fill-generator process)
    (change-scene (send fill-generator get-scene))))

(define fill-generator-menu (new menu-item% (label "Fill") (parent generator-menu) (callback fill-generator-callback)))

(define (uniform-random-fill-generator-callback menu evt)
  (let ([gen (make-object uniform-random-fill-generator% scene canvas tiles 10)])
    (send gen process)
    (send canvas draw)))

(define uniform-random-fill-generator-menu (new menu-item% (label "Randomly Place") (parent generator-menu) (callback uniform-random-fill-generator-callback)))
(define rooms null)
(define (rectangle-generator-callback menu evt)
  (let ([gen (make-object rectangle-generator% scene canvas tiles rooms)])
    (send gen process)
    (set! rooms (append rooms (list (send gen get-room))))
    (send canvas draw)))

(define rectangle-generator-menu (new menu-item% (label "Rectangle") (parent generator-menu) (callback rectangle-generator-callback)))

(define (room-connector-generator-callback menu evt)
  (let ([gen (make-object room-connector-generator% scene canvas tiles rooms (length rooms))])
    (send gen process)
    (send canvas draw)))

(define room-connector-generator-menu (new menu-item% (label "Room Connector") (parent generator-menu) (callback room-connector-generator-callback)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tool-pane (new pane%
	[parent frame]))

(define info-panel (new horizontal-panel% [parent tool-pane]))

(define (update-info-panel)
  (define t (send cur-brush get-selected-tile))
  (when (tile? t)
    (send info-symbol-field set-selection (symbol->integer (tile-symbol t)))
    (send info-descr-field set-value (tile-descr t))))

(define info-vpanel (new vertical-panel% [parent info-panel]))
(define info-symbol-field (new choice% [label "Symbol"] [choices cp437-strings] [parent info-vpanel]))
(define info-descr-field (new text-field% [label "Description"] [parent info-vpanel]))

(define info-bg (make-object color% 0 0 0 1.0))
(define info-fg (make-object color% 0 0 0 1.0))

(define info-fg-panel (new vertical-panel% [parent info-panel]))

(define info-fg-msg (new message%
                         (parent info-fg-panel)
                         (label "Foreground")))

(define info-fg-canvas (make-object (class canvas%
                                      (super-new [parent info-fg-panel])
                                      (define/public (color c)
                                        (send this set-canvas-background c)
                                        (send this redraw))
                                      (define/public (redraw)
                                        (send this refresh) (send this refresh-now)))))

(define info-bg-panel (new vertical-panel% [parent info-panel]))

(define info-bg-msg (new message%
                         (parent info-bg-panel)
                         (label "Background")))

(define info-bg-canvas (make-object (class canvas%
                                      (define/public (color c)
                                        (send this set-canvas-background c)
                                        (send this redraw))
                                      (define/public (redraw)
                                        (send this refresh) (send this refresh-now))
                                      (super-new [parent info-bg-panel]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define paint-brush (new paint-brush% [canvas canvas] [scene scene]))
(define single-brush (new single-brush% [canvas canvas] [scene scene]))
(define selection-brush (new selection-brush% [canvas canvas] [scene scene] [bg-canvas info-bg-canvas] [fg-canvas info-fg-canvas]))
(define line-brush (new line-brush% [canvas canvas] [scene scene]))
(define brushes (list paint-brush single-brush selection-brush line-brush))
(define cur-brush paint-brush)

(define create-tile-btn (new button% [label "Create Tile"] [parent brush-hpanel] 
                             [callback (thunk* 
                                        (send creator-fg-canvas refresh-now)
                                        (send creator-bg-canvas refresh-now)
                                        (send creator-descr-field set-value creator-next-id)
                                        (send creator-dialog show #t))]))

(define (switch-brush b)
  (set! cur-brush b)
  (send cur-brush set-tile cur-tile)
  (case (send cur-brush get-name)
    [("View") (send tile-panel show #f) (send info-panel show #t)]
    [else (send info-panel show #f) (send tile-panel show #t)]))

(define brush-paint-btn (new button% [label (send paint-brush get-name)] [parent brush-hpanel] 
	[callback (thunk* (switch-brush paint-brush))]))

(define brush-single-btn (new button% [label "Single"] [parent brush-hpanel] 
                              [callback (thunk* (switch-brush single-brush))]))

(define brush-line-btn (new button% [label "Line"] [parent brush-hpanel] 
                            [callback (thunk* (switch-brush line-brush))]))

(define brush-selection-btn (new button% [label "Selector"] [parent brush-hpanel] 
                                 [callback (thunk* (switch-brush selection-brush))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tile-panel (new horizontal-panel% [parent tool-pane]))

(define (change-tile choice evt)
  (set-cur-tile (list-ref tiles (send choice get-selection)))
  (send symbol-field set-selection (symbol->integer (tile-symbol cur-tile)))
  (send descr-field set-value (tile-descr cur-tile)))

(define tile-choices (new choice% [label "Tiles"] [parent tile-panel] [choices (map tile-descr tiles)] [callback change-tile]))

(define (update-tile! obj evt)
  (let*-values ([(l r) (split-at tiles (send tile-choices get-selection))]
                [(t) (tile (integer->char (send symbol-field get-selection))
                           (tile-fg (first r)) (tile-bg (first r))
                           (send descr-field get-value))]
                [(new-tiles) (append l (cons t (rest r)))])
    (set! tiles new-tiles)
    (send cur-brush set-tile t)
    (send fg-canvas refresh-now)
    (send bg-canvas refresh-now)))

(define vpanel (new vertical-panel% [parent tile-panel]))
(define symbol-field (new choice% [label "Symbol"] [choices cp437-strings] [parent vpanel] [callback update-tile!]))
(define descr-field (new text-field% [label "Description"] [parent vpanel] [callback update-tile!]))

(define fg-color-panel (new vertical-panel% [parent tile-panel]))

(define tile-fg-msg (new message%
                         (parent fg-color-panel)
                         (label "Foreground")))

(define fg-canvas (make-object (class canvas%
                                 (define/override (on-paint)
                                   (send this set-canvas-background (tile-fg cur-tile)))
                                 (define/public (redraw)
                                        (send this refresh) (send this refresh-now))
                                 (super-new [parent fg-color-panel]))))
(define bg-color-panel (new vertical-panel% [parent tile-panel]))
(define tile-bg-msg (new message%
                         (parent bg-color-panel)
                         (label "Background")))

(define bg-canvas (make-object (class canvas%
                                 (define/public (redraw)
                                        (send this refresh) (send this refresh-now))
     (define/override (on-paint)
       (send this set-canvas-background (tile-bg cur-tile)))
                                 (super-new [parent bg-color-panel]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define creator-next-num 1)
(define creator-next-id "")
(define (generate-id)
  (let ([id (string-append "Tile " (number->string creator-next-num))])
    (begin (set! creator-next-num (add1 creator-next-num)) (set! creator-next-id id) id)))

(define creator-dialog (new dialog% [label "New Tile"] [parent frame]))
(define creator-panel (new horizontal-panel% [parent creator-dialog]))

(define creator-vpanel (new vertical-panel% [parent creator-panel]))
(define creator-symbol-field (new choice% [label "Symbol"] [choices cp437-strings] [parent creator-vpanel] [callback update-tile!]))
(define creator-descr-field (new text-field% [label "Description"] [parent creator-vpanel] [callback update-tile!]))

(define (safe-get-color-from-user! label parent default-color)
  (let ([c (get-color-from-user label parent default-color)])
    (if (is-a? c color%) c default-color)))

(define creator-fg-panel (new vertical-panel% [parent creator-panel]))

(define creator-fg-canvas (make-object (class canvas%
                                         (super-new [parent creator-fg-panel])
                                         (define/public (redraw)
                                           (send this refresh) (send this refresh-now))
                                         (define/override (on-paint)
                                           (send this set-canvas-background creator-fg-color)))))

(define creator-fg-button (new button% [label "FG Color"] [parent creator-fg-panel] 
                               [callback (thunk* (set! creator-fg-color
                                                       (safe-get-color-from-user! "Foreground" frame creator-fg-color))
                                                 (send creator-fg-canvas redraw))]))

(define creator-bg-panel (new vertical-panel% [parent creator-panel]))

(define creator-bg-canvas (make-object (class canvas%
                                         (define/public (redraw)
                                           (send this refresh) (send this refresh-now))
                                         (define/override (on-paint)
                                           (send this set-canvas-background creator-bg-color))
                                         (super-new [parent creator-bg-panel]))))

(define creator-bg-button (new button% [label "BG Color"] [parent creator-bg-panel] 
                               [callback (thunk* (set! creator-bg-color
                                                       (safe-get-color-from-user! "Background" frame creator-fg-color))
                                                 (send creator-bg-canvas redraw))]))

(define (add-tile-callback btn evt)
  (let ([t (tile (integer->symbol (send creator-symbol-field get-selection))
                 creator-fg-color
                 creator-bg-color
                 (send creator-descr-field get-value))])
    (set! tiles (remove-duplicates (append tiles (list t))))
    (send tile-choices append (tile-descr t))
    (send tile-choices set-selection (sub1 (length tiles)))
    (set-cur-tile t)
   (send symbol-field set-selection (symbol->integer (tile-symbol cur-tile)))
  (send descr-field set-value (tile-descr cur-tile))
    (generate-id)
    (send creator-symbol-field set-selection 0)
    (set! creator-bg-color (make-object color% 0 0 0 1.0))
  (set! creator-fg-color (make-object color% 0 0 0 1.0))
  (send creator-dialog show #f)))

(define creator-button-panel (new vertical-panel% [parent creator-panel]))

(define creator-add-btn (new button% [label "Add Tile"] [parent creator-button-panel] [callback add-tile-callback]))

(define (randomize-tile-callback btn evt)
  (send creator-symbol-field set-selection (symbol->integer (get-random-symbol)))
  
  (set! creator-bg-color (get-random-color)) 
  (set! creator-fg-color (get-random-color))
  
  (send creator-fg-canvas redraw)
  (send creator-bg-canvas redraw)
  
  (send creator-dialog show #f)
  (send creator-dialog show #t))

(define creator-random-btn (new button% [label "Randomize Properties"] [parent creator-button-panel] [callback randomize-tile-callback]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define panels (list tile-panel info-panel))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (initialize)
  (generate-id)
  (send creator-fg-canvas redraw)
  (send creator-bg-canvas redraw)

  (send fg-canvas redraw)
  (send bg-canvas redraw)
    
  (send info-panel show #f)
  (send brush-hpanel set-alignment 'center 'center)
  
  (send descr-field set-value (tile-descr cur-tile))
  
  (send cur-brush set-tile cur-tile)
  
  (send canvas set-scales (/ (send canvas get-width) canvas-width) (/ (send canvas get-height) canvas-height))
  (send canvas draw)
  
  (send frame show #t))

(initialize)