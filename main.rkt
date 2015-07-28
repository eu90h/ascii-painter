#lang racket/gui

(require racket/serialize ascii-canvas file/gzip file/gunzip "scene.rkt" "symbol.rkt" "brush.rkt" "point.rkt" "util.rkt" "generator.rkt" "history.rkt" "interval.rkt")

(define history null)
(define rooms null)

(define camera-pos (pt 0 0))

(define fg-color (make-object color% 255 255 255 1.0))
(define bg-color (make-object color% 0 0 0 1.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define selection-tile (tile #\X (make-object color% 255 255 0 1.0) (make-object color% 0 0 0 1.0) "Crosshair"))

(define tiles (list empty-tile))

(define cur-tile empty-tile)
(define cur-tile-table-offset '(0 0))

(define (set-cur-tile t)
  (set! cur-tile t)
  (unless (eq? cur-brush selection-brush) (send cur-brush set-tile t))

  (set! fg-color (tile-fg t))
  (set! bg-color (tile-bg t))

  (send tile-fg-canvas redraw)
  (send tile-bg-canvas redraw))

(define (get-tile-index l v)
    (define (iter l i) 
      (if (null? l) -1 
        (if (eq? (tile-descr (first l)) v) i 
          (iter (rest l) (add1 i)))))
    (iter l 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define canvas-width 60)
(define canvas-height 35)

(define scene (new scene% [width canvas-width] [height canvas-height] [tile empty-tile]))

(define (scene-draw canvas scene) 
  (for* ([xi (in-range canvas-width)] [yi (in-range canvas-height)])
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
      (change-scene (new scene% [width w] [height h] [tile t]))))

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
  (close-output-port out))

(define load-menu (new menu-item% (label "Load") (parent file-menu) (callback load-callback)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define canvas-panel (new horizontal-panel% [parent frame]))
(define canvas-left-panel (new vertical-panel% [parent canvas-panel]))
(define symbol-canvas (new (class ascii-canvas%
  (super-new [parent canvas-left-panel] [width-in-characters 16] [height-in-characters 16])
  (field [x-scale (/ (send this get-width) 16)] [y-scale (/ (send this get-height) 16)])

  (field [width 16] [height 16] [x-interval (interval 0 (sub1 width))] [y-interval (interval 0 (sub1 height))]
    [data (for/vector ([x width]) 
            (for/vector ([y height]) 
              (tile (integer->symbol (modulo (+ (* 16 y) x) 255)) 
                    (make-object color% 255 255 255 1.0) 
                    (make-object color% 0 0 0 1.0)
                    (list-ref cp437-strings (modulo (+ (* 16 y) x) 255)))))])

  (define/public (set-scales x y) (set! x-scale x) (set! y-scale y))

  (define/public (clamp mx my)
    (pt (floor (/ mx x-scale))
        (floor (/ my y-scale))))

  (define (process-tiles fn) (for* ([x (in-range 16)] [y (in-range 16)])
    (set x y (fn (send this symbol-table-lookup x y)))))

  (define (set x y tile) (when (good-xy? x y) (vector-set! (vector-ref data x) y tile)))

  (define/public (dye-tiles fg bg)
    (process-tiles (lambda (t) (tile (tile-symbol t) fg bg (tile-descr t))))
    (draw-symbol-table)
    (send frame refresh))

  (define/override (on-event mouse-event)
    (when (eq? 'left-up (send mouse-event get-event-type))
    (let* ([p (send this clamp (send mouse-event get-x) (send mouse-event get-y))])
      (set! cur-tile-table-offset (list (pt-x p) (pt-y p)))
      (set-cur-tile (symbol-table-lookup (pt-x p) (pt-y p))))))

  (define/public (draw) 
    (draw-symbol-table) 
    (send frame refresh))

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
      (draw-tile (symbol-table-lookup x y) x y))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define canvas (new (class ascii-canvas%
  (super-new [parent canvas-panel] [width-in-characters canvas-width] [height-in-characters canvas-height])

  (field [x-interval (interval 0 (sub1 canvas-width))] [y-interval (interval 0 (sub1 canvas-height))])
  (field [x-scale (/ (send this get-width) canvas-width)])
  (field [y-scale (/ (send this get-height) canvas-height)])
  (field [last-mouse-pt (pt 0 0)])

  (define/public (set-scales x y) (set! x-scale x) (set! y-scale y))
  (define/public (clamp mx my)
    (pt (+ (pt-x camera-pos) (floor (/ mx x-scale)))
        (+ (pt-y camera-pos) (floor (/ my y-scale)))))

  (define (within-scene-bounds? p)
    (let* ( [x (pt-x p)] [y (pt-y p)])
      (and (>= x 0) (>= y 0) (< x (sub1 (send scene get-width))) (< y  (send scene get-height)))))

  (define (valid-camera-pos? p)
      (and (within-scene-bounds? p) (within-scene-bounds? (pt-add p (pt canvas-width canvas-height)))))

  (define (safe-add p)
    (if (valid-camera-pos? (pt-add camera-pos p)) (pt-add camera-pos p) camera-pos))

  (define/override (on-char key-event)
    (case (send key-event get-key-code)
      [(menu release) (void)]
      [(escape) (if (eq? 'yes (message-box "Exit" "Are you sure you want to exit?" frame '(yes-no))) (exit) (void))]
      [(#\z)  (set! history (undo-last-action history scene)) (send this draw)]
      [(up #\w) (set! camera-pos (safe-add  (pt 0 -1))) (send this draw)]
      [(left #\a) (set! camera-pos (safe-add  (pt -1 0))) (send this draw)]
      [(down #\s) (set! camera-pos (safe-add  (pt 0 1))) (send this draw)]
      [(right #\d) (set! camera-pos (safe-add  (pt 1 0))) (send this draw)])
    this)

  (define/override (on-event mouse-event)
    (let ([p (send this clamp (send mouse-event get-x) (send mouse-event get-y))])
      (when (within-scene-bounds? p)
        (draw-tile
          (send scene get (pt-x last-mouse-pt) (pt-y last-mouse-pt)) (pt-x last-mouse-pt) (pt-y last-mouse-pt))
        (let ([q (pt-sub p camera-pos)]) 
          (draw-tile (if (eq? cur-brush selection-brush) selection-tile cur-tile) (pt-x q) (pt-y q))
          (send frame refresh))
        (set! last-mouse-pt (pt-sub p camera-pos))))
    (unless (eq? cur-brush selection-brush) 
      (send cur-brush handle mouse-event) 
      (set! history (history-add-actions history (send cur-brush get-history)))))

  (define/public (get-width-in-chars) canvas-width)

  (define (good-xy? x y) 
    (and (number-in-interval? x x-interval) (number-in-interval? y y-interval)))

  (define/public (draw-tile tile canvas-x canvas-y)
    (when (good-xy? canvas-x canvas-y)
      (send this write (tile-symbol tile) canvas-x canvas-y (tile-fg tile) (tile-bg tile))))

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

(define fg-color-panel (new vertical-panel% [parent canvas-left-panel]))

(define tile-fg-msg (new message% [parent fg-color-panel] [label "Foreground"]))

(define tile-fg-canvas (make-object (class canvas%
  (define/override (on-paint)
    (send this set-canvas-background fg-color))
  (define/public (redraw)
    (send this refresh) (send this refresh-now))
  (super-new [parent fg-color-panel]))))

(define (change-fg-btn-callback btn evt)
  (set! fg-color (safe-get-color-from-user! "Foreground" frame fg-color))
  (send symbol-canvas dye-tiles fg-color bg-color)
  (set-cur-tile 
    (send symbol-canvas symbol-table-lookup (first cur-tile-table-offset) (second cur-tile-table-offset)))
  (send tile-fg-canvas redraw))


(define bg-color-panel (new vertical-panel% [parent canvas-left-panel]))

(define tile-bg-msg (new message% [parent bg-color-panel] [label "Background"]))

(define tile-bg-canvas (make-object (class canvas%
  (define/public (redraw)
    (send this refresh) (send this refresh-now))
  (define/override (on-paint)
    (send this set-canvas-background bg-color))
  (super-new [parent bg-color-panel]))))

(define (change-bg-btn-callback btn evt)
  (set! bg-color (safe-get-color-from-user! "Background" frame bg-color))
  (send symbol-canvas dye-tiles fg-color bg-color)
  (set-cur-tile 
    (send symbol-canvas symbol-table-lookup (first cur-tile-table-offset) (second cur-tile-table-offset)))
  (send tile-bg-canvas redraw))
  
(define change-fg-btn (new button% [parent fg-color-panel] [label "Change Foreground"] 
  [callback change-fg-btn-callback]))

(define change-bg-btn (new button% [parent bg-color-panel] [label "Change Background"] 
  [callback change-bg-btn-callback]))

(define tile-library-panel (new vertical-panel% [parent canvas-left-panel] [style (list 'border)]))
(define tile-choices-callback (thunk*
   (set-cur-tile (list-ref tiles (get-tile-index tiles (send tile-choices get-string-selection))))))
(define tile-choices (new choice% [parent tile-library-panel] [label "Saved Tiles"] [choices (map tile-descr tiles)]
  [callback tile-choices-callback]))

(define save-tile-btn-callback (thunk*
  (define dialog (new dialog% [label "Save Tile"]))
  (define hpanel (new horizontal-panel% [parent dialog]))
  (define name-field (new text-field% [label "Enter a tile name"] [parent hpanel]))

  (define save-tile (thunk*
    (let* ([name (send name-field get-value)] [t (tile (tile-symbol cur-tile) (tile-fg cur-tile) (tile-bg cur-tile) name)])
      (set! tiles (append tiles (list t)))
      (send tile-choices append name)
      (send tile-choices set-selection (sub1 (length tiles)))
      (send dialog show #f))))
   
   (define ok-btn (new button% [label "OK"] [parent hpanel] [callback save-tile]))
   (send dialog show #t)))

(define save-tile-btn (new button% [parent tile-library-panel] [label "Save Current Tile"]
  [callback save-tile-btn-callback]))

(define remove-tile-btn-callback (thunk*
  (let* ([t (list-ref tiles (get-tile-index tiles (send tile-choices get-string-selection)))]
    [remove? (eq? 'yes 
      (message-box "Remove Tile" (string-append "Are you sure you want to remove " (tile-descr t)) frame '(yes-no)))])
    (when (and remove? (not (eq? "empty" (tile-descr t))))
      (send tile-choices delete (send tile-choices get-selection))
      (set! tiles (filter (lambda (v) (not (eq? (tile-descr v) (tile-descr t)))) tiles))))
  (void)))

(define remove-tile-btn (new button% [parent tile-library-panel] [label "Delete Selected Tile"]
  [callback remove-tile-btn-callback]))

(define (safe-get-color-from-user! label parent default-color)
  (let ([c (get-color-from-user label parent default-color)])
    (if (is-a? c color%) c default-color)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define paint-brush (new paint-brush% [canvas canvas] [scene scene]))
(define single-brush (new single-brush% [canvas canvas] [scene scene]))
(define selection-brush #t)
(define line-brush (new line-brush% [canvas canvas] [scene scene]))
(define brushes (list paint-brush single-brush line-brush))
(define cur-brush paint-brush)

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
  (send tile-fg-canvas min-height 50)
  (send tile-bg-canvas min-height 50)

  (send tile-fg-canvas redraw)
  (send tile-bg-canvas redraw)
    
  (send brush-hpanel set-alignment 'center 'center)
  
  (send cur-brush set-tile cur-tile)

  (send symbol-canvas min-width 256)
  (send symbol-canvas min-height 256)
  (send symbol-canvas set-scales 16 16)
  (send symbol-canvas draw)

  (send frame show #t)

  (send canvas set-scales (/ (send canvas get-width) canvas-width) (/ (send canvas get-height) canvas-height))
  (send canvas draw))

(initialize)