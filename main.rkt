#lang racket/gui

(require racket/serialize ascii-canvas file/gzip file/gunzip "scene.rkt" "symbol.rkt" "brush.rkt" "point.rkt" "util.rkt" "generator.rkt" "history.rkt")

(define camera-pos (pt 0 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tiles (list empty-tile))
(define selection-tile (tile #\X (make-object color% 255 255 0 1.0) (make-object color% 0 0 0 1.0) "Crosshair"))
(define cur-tile (first tiles))
(define (set-cur-tile t)
  (set! cur-tile t)
  (unless (eq? cur-brush selection-brush) (send cur-brush set-tile t))
  (send tile-fg-canvas redraw)
  (send tile-bg-canvas redraw))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define canvas-width 70)
(define canvas-height 30)

(define scene (new scene% [width canvas-width] [height canvas-height] [tile empty-tile]))

(define (scene-draw canvas scene) (for* ([xi (in-range canvas-width)] [yi (in-range canvas-height)])
                                    (send canvas draw-tile (send scene get (+ (pt-x camera-pos) xi) (+ (pt-y camera-pos) yi))  xi  yi)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define frame (new frame% [label "Scene Editor"] [style '(no-resize-border)]))
(define brush-hpanel (new horizontal-panel% [parent frame]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define menu-bar (new menu-bar% (parent frame)))
(define file-menu (new menu% (label "&File") (parent menu-bar)))
(define generator-menu (new menu% (label "&Generators") (parent menu-bar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-callback menu evt) 
  (define dialog (new dialog% [label "New Scene"]))
  (define hpanel (new horizontal-panel% [parent dialog]))
  (define width-field (new text-field% [label "Width"] [parent hpanel]))
  (define height-field (new text-field% [label "Height"] [parent hpanel]))
  (define tile-choices (new choice% [label "Tiles"] [parent hpanel] [choices (map tile-descr tiles)]))
  (define choice->tile ((curry list-ref) tiles))
  (define (make-new-scene btn evt)
    (send dialog show #f)
    (let ([w (string->number (send width-field get-value))] 
          [h (string->number (send height-field get-value))]
          [t (choice->tile (send tile-choices get-selection))])
      (change-scene (new scene% [width w] [height h] [tile t])))
    (send tile-choices set-selection 0)
    (change-tile tile-choices evt)
    (set! creator-next-num (send tile-choices get-number))
    (generate-id))

  (define ok-btn (new button% [label "OK"] [parent hpanel] [callback make-new-scene]))
  (send dialog show #t))

(define new-menu (new menu-item% (label "New") (parent file-menu) (callback new-callback)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (save-callback menu evt) 
  (let ([path (put-file)] [tmp (make-temporary-file)])
    (define out (open-output-file tmp #:mode 'binary #:exists 'replace))
    (write (serialize-scene (send scene copy)) out)
    (close-output-port out)
    (gzip (path->string tmp) (path->string path))))

(define save-menu (new menu-item% (label "Save") (parent file-menu) (callback save-callback)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (load-callback menu evt)
  (define in (open-input-file (get-file) #:mode 'binary))
  (define out (open-output-string))
  (gunzip-through-ports in out)
  (close-input-port in)
  (change-scene (deserialize-scene (read (open-input-string (get-output-string out)))))
  (close-output-port out)
  (let* ([loaded-tiles (remove-duplicates (flatten (map vector->list (vector->list (send scene get-data))))
                                          (lambda (t1 t2) (eq? (tile-descr t1) (tile-descr t2))))] 
    [reordered-tiles (append (list (first loaded-tiles)) (reverse (rest loaded-tiles)))])
    (set! tiles reordered-tiles)
    (send tile-choices clear)
   
    (for ([i (in-range (length reordered-tiles))])
      (send tile-choices append (tile-descr (list-ref reordered-tiles i)))))

  (send tile-choices set-selection 0)
  (change-tile tile-choices evt)
  (set! creator-next-num (send tile-choices get-number))
  (generate-id))

(define load-menu (new menu-item% (label "Load") (parent file-menu) (callback load-callback)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define canvas (new (class ascii-canvas%
                      (super-new [parent frame] [width-in-characters canvas-width] [height-in-characters canvas-height])
                      
                      (field [x-scale (/ (send this get-width) canvas-width)])
                      (field [y-scale (/ (send this get-height) canvas-height)])
                      (field [last-mouse-pt (pt 0 0)])

                      (define/public (set-scales x y) (set! x-scale x) (set! y-scale y))
                      (define/public (clamp mx my)
                        (pt (+ (pt-x camera-pos) (round (/ mx x-scale)))
                            (+ (pt-y camera-pos) (round (/ my y-scale)))))
                 
                      (define (within-scene-bounds? p)
                        (let* ( [x (pt-x p)] [y (pt-y p)])
                          (and (>= x 0) (>= y 0) (< x (sub1 (send scene get-width))) (< y  (send scene get-height)))))

                      (define (valid-camera-pos? p)
                          (and (within-scene-bounds? p) (within-scene-bounds? (pt-add p (pt canvas-width canvas-height)))))

                      (define (safe-add p)
                        (if (valid-camera-pos? (pt-add camera-pos p)) (pt-add camera-pos p) camera-pos))

                      (field [holding-control #f])

                      (define/override (on-char key-event)
                        (case (send key-event get-key-code)
                          [(menu release) (void)]
                          [(control) (set! holding-control (not holding-control))]
                          [(escape) (if (eq? 'yes (message-box "Exit" "Are you sure you want to exit?" frame '(yes-no))) (exit) (void))]
                          [(#\z)  (undo-last-action scene) (send this draw)]
                          [(up #\w) (set! camera-pos (safe-add  (pt 0 -1))) (send this draw)]
                          [(left #\a) (set! camera-pos (safe-add  (pt -1 0))) (send this draw)]
                          [(down #\s) (set! camera-pos (safe-add  (pt 0 1))) (send this draw)]
                          [(right #\d) (set! camera-pos (safe-add  (pt 1 0))) (send this draw)])
                        this)
                      
                      (define/override (on-event mouse-event)
                        (send frame refresh)
                        (let ([p (send this clamp (send mouse-event get-x) (send mouse-event get-y))])
                          (when (within-scene-bounds? p)
                            (draw-tile
                              (send scene get (pt-x last-mouse-pt) (pt-y last-mouse-pt)) (pt-x last-mouse-pt) (pt-y last-mouse-pt))
                            
                            (let ([q (pt-sub p camera-pos)]) 
                              (draw-tile  (if (eq? cur-brush selection-brush) selection-tile cur-tile) (pt-x q) (pt-y q))
                              (when (eq? cur-brush selection-brush) (update-info-panel q)))

                            (set! last-mouse-pt (pt-sub p camera-pos))))
                        (unless (eq? cur-brush selection-brush) (send cur-brush handle mouse-event)))
                  
                      
                      (define/public (get-width-in-chars) canvas-width)

                      (define/public (draw-tile tile canvas-x canvas-y)
                        (send this write (tile-symbol tile) canvas-x canvas-y (tile-fg tile) (tile-bg tile)))

                      (define/public (draw-selected-tiles)
                        (scene-draw this scene)
                        (define selected-points (send cur-brush get-selected-points))
                        (for ([i (in-range (length selected-points))])
                          (define p (list-ref selected-points i))
                          (define t (send scene get (pt-x p) (pt-y p)))
                          (send this draw-tile (tile (tile-symbol t) (make-object color% 255 255 0 1.0) (tile-bg t) (tile-descr t))
                            (+ (pt-x camera-pos) (pt-x p))
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

(define (update-info-panel mouse-click-coords)
  (let ([t (send scene get (pt-x mouse-click-coords) (pt-y mouse-click-coords))])
    (when (tile? t)
      (send tile-choices set-selection (modulo (send tile-choices find-string (tile-descr t)) (send tile-choices get-number)))
      (change-tile tile-choices null))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tile-panel (new horizontal-panel% [parent tool-pane]))

(define (change-tile choice evt)
  (set-cur-tile (list-ref tiles (send choice get-selection)))
  (send symbol-field set-selection (symbol->integer (tile-symbol cur-tile)))
  (send descr-field set-value (tile-descr cur-tile))
  (send tile-fg-canvas refresh-now)
  (send tile-bg-canvas refresh-now))

(define tile-choices (new choice% [label "Tiles"] [parent tile-panel] [choices (map tile-descr tiles)] [callback change-tile]))

(define (update-tile! obj evt)
  (let*-values ([(l r) (split-at tiles (send tile-choices get-selection))]
                [(t) (tile (integer->char (send symbol-field get-selection))
                           (tile-fg (first r)) (tile-bg (first r))
                           (send descr-field get-value))]
                [(new-tiles) (append l (cons t (rest r)))])
    (set! tiles new-tiles)
    (define selection (send tile-choices get-selection))
    (send tile-choices clear)
    (for ([i (in-range (length tiles))])
      (send tile-choices append (tile-descr (list-ref tiles i))))
    (send tile-choices set-selection selection)
    (send cur-brush set-tile t)
    (send tile-fg-canvas refresh-now)
    (send tile-bg-canvas refresh-now)))

(define vpanel (new vertical-panel% [parent tile-panel]))
(define symbol-field (new choice% [label "Symbol"] [choices cp437-strings] [parent vpanel] [callback update-tile!]))
(define descr-field (new text-field% [label "Description"] [parent vpanel] [callback update-tile!]))

(define fg-color-panel (new vertical-panel% [parent tile-panel]))

(define tile-fg-msg (new message%
                         (parent fg-color-panel)
                         (label "Foreground")))

(define tile-fg-canvas (make-object (class canvas%
                                 (define/override (on-paint)
                                   (send this set-canvas-background (tile-fg cur-tile)))
                                 (define/public (redraw)
                                        (send this refresh) (send this refresh-now))
                                 (super-new [parent fg-color-panel]))))
(define (change-fg-btn-callback btn evt)
  (set-cur-tile
  (tile (tile-symbol cur-tile)
    (safe-get-color-from-user! "Foreground" frame (tile-fg cur-tile))
    (tile-bg cur-tile)
    (tile-descr cur-tile)))
  (send creator-fg-canvas redraw))


(define bg-color-panel (new vertical-panel% [parent tile-panel]))
(define tile-bg-msg (new message%
                         (parent bg-color-panel)
                         (label "Background")))

(define tile-bg-canvas (make-object (class canvas%
                                 (define/public (redraw)
                                        (send this refresh) (send this refresh-now))
     (define/override (on-paint)
       (send this set-canvas-background (tile-bg cur-tile)))
                                 (super-new [parent bg-color-panel]))))

(define (change-bg-btn-callback btn evt)
  (set-cur-tile
  (tile (tile-symbol cur-tile)
    (tile-fg cur-tile)
    (safe-get-color-from-user! "Background" frame (tile-bg cur-tile))
    (tile-descr cur-tile)))
  (send creator-bg-canvas redraw))
  
(define change-fg-btn (new button% [parent fg-color-panel] [label "Change Foreground"] 
  [callback change-fg-btn-callback]))

(define change-bg-btn (new button% [parent bg-color-panel] [label "Change Background"] 
  [callback change-bg-btn-callback]))

(define (delete-tile-btn-callback btn evt)
  (send tile-choices delete (send tile-choices get-selection))
  (set-cur-tile (list-ref tiles (send tile-choices get-selection)))
  (send symbol-field set-selection (symbol->integer (tile-symbol cur-tile)))
  (send descr-field set-value (tile-descr cur-tile)))

(define delete-tile-btn (new button% [parent tile-panel] [label "Delete Tile"]
  [callback delete-tile-btn-callback]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define creator-next-num 1)
(define creator-next-id "")

(define (generate-id)
  (let ([id (string-append "Tile " (number->string creator-next-num))])
    (begin (set! creator-next-num (add1 creator-next-num)) (set! creator-next-id id) id)))

(define creator-fg-color (make-object color% 100 100 100))
(define creator-bg-color (make-object color% 100 100 100))

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
                                                       (safe-get-color-from-user! "Background" frame creator-bg-color))
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

(define paint-brush (new paint-brush% [canvas canvas] [scene scene]))
(define single-brush (new single-brush% [canvas canvas] [scene scene]))
(define selection-brush #t)
(define line-brush (new line-brush% [canvas canvas] [scene scene]))
(define brushes (list paint-brush single-brush line-brush))
(define cur-brush paint-brush)

(define create-tile-btn (new button% [label "Create Tile"] [parent brush-hpanel] 
                             [callback (thunk* 
                                        (send creator-fg-canvas refresh-now)
                                        (send creator-bg-canvas refresh-now)
                                        (send creator-descr-field set-value creator-next-id)
                                        (send creator-dialog show #t))]))

(define (switch-brush b)
  (set! cur-brush b)
  (unless (eq? cur-brush selection-brush) (send cur-brush set-tile cur-tile)))

(define brush-paint-btn (new button% [label (send paint-brush get-name)] [parent brush-hpanel] 
  [callback (thunk* (switch-brush paint-brush))]))

(define brush-single-btn (new button% [label "Single"] [parent brush-hpanel] 
                              [callback (thunk* (switch-brush single-brush))]))

(define brush-line-btn (new button% [label "Line"] [parent brush-hpanel] 
                            [callback (thunk* (switch-brush line-brush))]))

(define brush-selection-btn (new button% [label "Selector"] [parent brush-hpanel] 
                                 [callback (thunk* (switch-brush selection-brush))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (initialize)
  (generate-id)

  (send creator-fg-canvas redraw)
  (send creator-bg-canvas redraw)

  (send tile-fg-canvas min-height 50)
  (send tile-bg-canvas min-height 50)

  (send tile-fg-canvas redraw)
  (send tile-bg-canvas redraw)
    
  (send brush-hpanel set-alignment 'center 'center)
  
  (send descr-field set-value (tile-descr cur-tile))
  
  (send cur-brush set-tile cur-tile)
  
  (send canvas set-scales (/ (send canvas get-width) canvas-width) (/ (send canvas get-height) canvas-height))
  (send canvas draw)
  
  (send frame show #t))

(initialize)