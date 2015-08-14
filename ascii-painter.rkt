#lang racket/gui

(require racket/serialize
         file/gzip
         file/gunzip
         "ascii-canvas.rkt"  
         "scene.rkt"
         "brush.rkt"
         "point.rkt"
         "util.rkt" 
         "generator.rkt"
         "history.rkt"
         "camera.rkt"
         "main-canvas.rkt"
         "symbol-canvas.rkt"
         "cerialize.rkt")

(define history null)
(define rooms null)

(define fg-color (make-object color% 255 255 255 1.0))
(define bg-color (make-object color% 0 0 0 1.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tiles (list empty-tile))

(define cur-tile empty-tile)
(define cur-tile-table-offset '(0 0))

(define (set-cur-tile t)
  (set! cur-tile t)
  (send canvas set-cur-tile t)
  (send cur-brush set-tile t)

  (set! fg-color (tile-fg t))
  (set! bg-color (tile-bg t))
  
  (send symbol-canvas dye-tiles fg-color bg-color)

  (send tile-fg-canvas redraw)
  (send tile-bg-canvas redraw))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define canvas-width 60)
(define canvas-height 40)

(define default-scene-width 100)
(define default-scene-height 60)

(define title 
  (string-append "Ascii-Painter - New Scene (" (number->string default-scene-width) "x" (number->string default-scene-height) ")"))

(define scene (new scene% [width default-scene-width] [height default-scene-height] [tile empty-tile]))

(define camera 
  (make-object camera% (pt 0 0) default-scene-width default-scene-height canvas-width canvas-height))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define frame (new frame%
                   [label title] [style '(no-resize-border)]))
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
      (change-scene (new scene% [width w] [height h] [tile t]) "")))
  
  (define ok-btn (new button% [label "OK"] [parent hpanel] [callback make-new-scene]))
  (send dialog show #t))


(define new-menu (new menu-item% (label "New") (parent file-menu) (callback new-callback)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (save-callback menu evt) 
  (let ([path (put-file)] [tmp (make-temporary-file)])
    (define w (send scene get-width))
    (define h (send scene get-height))
    (send frame set-label 
          (string-append "Ascii-Painter - " 
                         (last (string-split (path->string path) "/")) " (" (number->string w) "x" (number->string h) ")"))
    (define out (open-output-file path #:exists 'replace))
    (cerialize-number w out) 
    (display "," out)
    (cerialize-number h out)
    (newline out)
    (for* ([x (in-range 0 w)] [y (in-range 0 h)])
      (cerialize-struct (send scene get x y) 4 out))

    (close-output-port out)))

(define save-menu (new menu-item% (label "Save") (parent file-menu) (callback save-callback)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (load-callback menu evt)
  (define path (get-file))
  (define in (open-input-file path #:mode 'binary))
  (define dimensions (map string->number (string-split (read-line in) ",")))
  (define new-scene (make-object scene% (first dimensions) (second dimensions) empty-tile))
  (for* ([x (in-range 0 (first dimensions))] [y (in-range 0 (second dimensions))])
    (define t (uncerialize-line (read-line in)))
    (when (null? (filter (lambda (v) (tile-equal? v t)) tiles))
        (set! tiles (append tiles (list t)))
        (send tile-choices append 
              (string-append 
               (tile-descr t) 
               "  #" 
               (number->string 
                (length
                 (filter (lambda (v) (equal? v (tile-symbol t))) 
                         (map tile-symbol tiles))))))'
        (send tile-choices set-selection (sub1 (length tiles))))
   
    (send new-scene set x y t))
  (change-scene 
   new-scene
   (last (string-split (path->string path) "/")))
  
  (close-input-port in))

(define load-menu (new menu-item% (label "Load") (parent file-menu) (callback load-callback)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define canvas-panel (new horizontal-panel% [parent frame]))

(define canvas-left-panel (new vertical-panel% [parent canvas-panel]))

(define save-tile-btn-callback (thunk*
                                (define dialog (new (class dialog% (init label)
                                                      (super-new [label label])
                                                      (define/override (on-subwindow-char receiver event)
                                                        (case (send event get-key-code)
                                                          [(#\return)
                                                            (save-tile)]
                                                          [(escape) (send this show #f)
                                                                    (send frame focus)]
                                                          [else #f])))
                                                    [label "Save Tile"]))
                                (define hpanel (new horizontal-panel% [parent dialog]))
                                (define name-field (new text-field% [label "Enter a tile name"] [parent hpanel]))
                                
                                (define save-tile (thunk*
                                                   (cond [(eq? "" (send name-field get-value)) 
                                                          (let ([d (new dialog% [label "Error"])])
                                                            (message-box "Error" "The tile must have a name" d '(ok))
                                                            (send d show #f))]
                                                         [(list? (member (send name-field get-value) (map tile-descr tiles))) 
                                                          (let ([d (new dialog% [label "Error"])])
                                                            (message-box "Error" "A tile with that name is already saved" d '(ok))
                                                            (send d show #f))]
                                                         [else 
                                                          (let* ([name (send name-field get-value)] [t (tile (tile-symbol cur-tile) (tile-fg cur-tile) (tile-bg cur-tile) name)])
                                                            (set! tiles (append tiles (list t)))
                                                            (send tile-choices append name)
                                                            (send tile-choices set-selection (sub1 (length tiles)))
                                                            (send dialog show #f))])
                                                   (send frame focus)))
                                (define ok-btn (new button% [label "OK"] [parent hpanel] [callback save-tile]))
                                (send dialog show #t)))

(define symbol-canvas (new symbol-canvas%
                           [container canvas-left-panel]
                           [scene scene]
                           [save-tile save-tile-btn-callback]
                           [set-tile-callback (lambda (t offset)
                                                (set-cur-tile t)
                                                (set! cur-tile-table-offset offset))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (change-scene s [scene-name null])
  (set! scene s)
  (define w (send s get-width))
  (define h (send s get-height))
  (send frame set-label 
        (if (null? scene-name)
            title
            (string-append "Ascii-Painter - " scene-name
                           " (" (number->string w) "x" (number->string h) ")")))
  (map (lambda (b) (send b set-scene s)) brushes)
  (send camera set-position 0 0)
  (send canvas set-scene s)
  (send camera set-scene-dimensions w h)
  (send canvas scene-draw))

(define (fill-generator-callback menu evt)
  (let ([fill-generator (make-object fill-generator% scene canvas tiles)])
    (send fill-generator process)
    (when (not (null? (send fill-generator get-scene))) 
      (change-scene (send fill-generator get-scene))
      (send frame focus))))

(define fill-generator-menu (new menu-item%
                                 (label "Fill")
                                 (parent generator-menu)
                                 (callback fill-generator-callback)))

(define (uniform-random-fill-generator-callback menu evt)
  (let ([gen (make-object uniform-random-fill-generator% scene canvas tiles 10)])
    (send gen process)
    (send canvas scene-draw)
    (send frame focus)))

(define uniform-random-fill-generator-menu (new menu-item% (label "Randomly Place") (parent generator-menu) (callback uniform-random-fill-generator-callback)))

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
(define tile-choices-callback
  (thunk*
  (set-cur-tile (list-ref tiles (send tile-choices get-selection)))))     
(define tile-choices (new choice% [parent tile-library-panel]
                          [label "Saved Tiles"]
                          [choices (map tile-descr tiles)]
                          [callback tile-choices-callback]))

(define save-tile-btn (new button% [parent tile-library-panel] [label "Save Current Tile"]
  [callback save-tile-btn-callback]))

(define remove-tile-btn-callback (thunk*
                                  (let* ([t (list-ref tiles (send tile-choices get-selection))]
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
(define canvas (new main-canvas%
                    [container canvas-panel]
                    [width canvas-width]
                    [height canvas-height]
                    [scene scene]
                    [camera camera]
                    [cur-tile cur-tile]
                    [history history]
                    [save-tile save-tile-btn-callback]
                    [set-tile-callback set-cur-tile]))

(define paint-brush (new paint-brush% [canvas canvas] [scene scene]))
(define single-brush (new single-brush% [canvas canvas] [scene scene]))
(define shape-brush (new shape-brush% [canvas canvas] [scene scene]))
(define select-brush (new select-brush% [canvas canvas] [scene scene]))
(define brushes (list paint-brush single-brush shape-brush))
(define cur-brush paint-brush)

(define (switch-brush b)
  (send canvas unselect-all)
  (set! cur-brush b)
  (send canvas set-brush b)
  (send cur-brush set-tile cur-tile)
  (send shape-size-slider show (and (eq? cur-brush shape-brush)
                                    (false? (member (send cur-brush get-shape) (list "line" "filled-rectangle" "rectangle"))))))

(define brush-paint-btn (new button% [label (send paint-brush get-name)] [parent brush-hpanel] 
                             [callback (thunk* (switch-brush paint-brush))]))

(define brush-single-btn (new button% [label "Single"] [parent brush-hpanel] 
                              [callback (thunk* (switch-brush single-brush))]))
(define brush-select-btn (new button% [label "Select"] [parent brush-hpanel] 
                              [callback (thunk* (switch-brush select-brush))]))
(define (shape-choice-callback btn evt)
  (switch-brush shape-brush)
  (define shape (send shape-choice get-string (send shape-choice get-selection)))
  (send shape-brush set-shape shape)
  (send shape-size-slider show (and (eq? cur-brush shape-brush)
                                    (false? (member shape (list "line" "filled-rectangle" "rectangle"))))))

(define shape-choice (new choice% [label "Shapes"] [parent brush-hpanel] [choices (send shape-brush get-shapes)] [callback shape-choice-callback]))

(define (shape-size-slider-callback slider evt)
  (send shape-brush set-radius (send slider get-value)))

(define shape-size-slider (new slider%
                               (label "Radius")
                               (parent brush-hpanel)
                               (min-value 0)
                               (max-value 100)
                               (init-value 4)
                               [callback shape-size-slider-callback]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (initialize)
  (send shape-size-slider show #f)
  
  (switch-brush paint-brush)
  
  (send tile-fg-canvas min-height 50)
  (send tile-bg-canvas min-height 50)
  
  (send tile-fg-canvas redraw)
  (send tile-bg-canvas redraw)
  
  (send brush-hpanel set-alignment 'center 'center)
  
  (send symbol-canvas min-width 256)
  (send symbol-canvas min-height 256)
  (send symbol-canvas set-scales 16 16)
  (send symbol-canvas draw)
  
  (send frame show #t)
  (send canvas focus)
  
  (send canvas set-scales (/ (send canvas get-width) canvas-width) (/ (send canvas get-height) canvas-height))
  (send canvas scene-draw))
(initialize)