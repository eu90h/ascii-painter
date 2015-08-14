#lang racket

(provide paint-brush% single-brush%  
  brush-interface brush-with-selection-interface shape-brush% select-brush%)

(require "scene.rkt" "util.rkt" "point.rkt" "history.rkt" racket/performance-hint racket/unsafe/ops)

(define brush-interface (interface () get-name set-scene set-tile set-canvas handle get-history set-history)) 
(define brush-with-selection-interface (interface (brush-interface) get-selected-points))

(define single-brush% (class* object% (brush-with-selection-interface)
                        (init-field canvas scene)
                        
                        (field [history null]
                               [tile empty-tile]
                               [selected-points null] 
                               [width (send scene get-width)]
                               [height (send scene get-height)])
                        
                        (super-new)
                        
                        (define/public (set-history h) (set! history h))
                        (define/public (get-history) (let ([h history]) (begin (set! history null) h)))
                        (define/public (get-name) "Single")
                        (define/public (set-scene c) (set! scene c))
                        (define/public (set-tile t) (set! tile t))
                        (define/public (set-canvas c) (set! canvas c))
                        (define/public (get-selected-points) selected-points)
                        
                        (begin-encourage-inline (define (good-xy? x y)
                                                  (and (unsafe-fx>= y 0) (unsafe-fx>= x 0) (unsafe-fx< x width) (unsafe-fx< y height))))
                        
                        (define (change-tile x y)
                          (let ([p (send canvas clamp x y)])
                            (when (good-xy? (pt-x p) (pt-y p))
                              (set! history (paint-scene history scene canvas (pt-x p) (pt-y p) tile)))))
                        
                        (define (remove-tile x y) 
                          (let ([p (send canvas clamp x y)]) 
                            (when (good-xy? (pt-x p) (pt-y p))
                             (set! history (paint-scene history scene canvas (pt-x p) (pt-y p) empty-tile)))))
                        
                        (define/public (handle mouse-event)
                          (when (eq? 'right-up (send mouse-event get-event-type))
                            (remove-tile (send mouse-event get-x) (send mouse-event get-y)))
                          (when (eq? 'left-up (send mouse-event get-event-type))
                            (change-tile (send mouse-event get-x) (send mouse-event get-y))))))

(define paint-brush% (class* object% (brush-interface)
                       (init-field canvas scene)
                       
                       (field [history null] [tile empty-tile] [drawing #f] [removing #f]
                              [width (send scene get-width)] [height (send scene get-height)])
                       
                       (super-new)
                       
                       (define/public (set-history h) (set! history h))
                       (define/public (get-history) (let ([h history]) (begin (set! history null) h)))
                       (define/public (get-name) "Paint")
                       (define/public (set-scene c) (set! scene c))
                       (define/public (set-tile t) (set! tile t))
                       (define/public (set-canvas c) (set! canvas c))
                       
                       (begin-encourage-inline (define (good-xy? x y)
                                                 (and (unsafe-fx>= y 0) (unsafe-fx>= x 0) (unsafe-fx< x width) (unsafe-fx< y height))))

                       (define (remove-tile x y) 
                         (let ([p (send canvas clamp x y)])
                          (when (good-xy? (pt-x p) (pt-y p))
                           (set! history (paint-scene history scene canvas (pt-x p) (pt-y p) empty-tile)))))

                       (define (change-tile x y)
                         (let ([p (send canvas clamp x y)])
                           (when (good-xy? (pt-x p) (pt-y p))
                             (set! history (paint-scene history scene canvas (pt-x p) (pt-y p) tile)))))
                       
                       (define/public (handle mouse-event)
                         (when drawing (change-tile (send mouse-event get-x) (send mouse-event get-y)))
                         (when removing (remove-tile (send mouse-event get-x) (send mouse-event get-y)))
                         (case (send mouse-event get-event-type)
                           [(left-down) (set! drawing #t) (set! removing #f)]
                           [(left-up) (set! drawing #f)]
                           [(right-down) (set! drawing #f) (set! removing #t)]
                           [(right-up) (set! removing #f)]))))

(define shape-brush% (class* object% (brush-interface)
                       (init-field canvas scene)

                       (field [tile empty-tile]
                              [history null]
                              [placing #f]
                              [initial-pt (pt 0 0)]
                              [tiles-drawn null]
                              [radius 4]
                              [tracer trace-line]
                              [shape "line"]
                              [shapes (list "line" "rectangle" "filled-rectangle" "circle" "weird-circle" "weird-rectangle" "weird-star" "diamond")]
                              [width (send scene get-width)]
                              [height (send scene get-height)]
                              [last-mouse-pt null])
                       
                       (super-new)

                       (begin-encourage-inline  (define (good-xy? x y)
                                                  (and (unsafe-fx>= y 0) (unsafe-fx>= x 0) (unsafe-fx< x width) (unsafe-fx< y height))))
                       
                       (begin-encourage-inline (define (select-tile x y)
                         (send canvas select x y)))
                       
                       (define (set-and-accumulate x y)
                         (when (good-xy? x y)
                            (set! tiles-drawn (append tiles-drawn (list (list (send scene get x y) x y))))
                            (send scene set x y tile)))
                       
                       (define (handle-left-down mouse-event)
                         (set! placing #t)
                         (set! initial-pt (evt-clamp canvas mouse-event)))
                       
                       (define (build-tracer-args mouse-event)
                         (case shape
                           [("line") (list set-and-accumulate initial-pt (evt-clamp canvas mouse-event))]
                           [("filled-rectangle" "rectangle") (list set-and-accumulate initial-pt (evt-clamp canvas mouse-event))]
                           [("circle" "weird-circle" "weird-rectangle" "weird-star" "diamond") (list set-and-accumulate (evt-clamp canvas mouse-event) radius)]))

                       (define (build-selector-args mouse-event)
                         (case shape
                           [("line") (list select-tile initial-pt (evt-clamp canvas mouse-event))]
                           [("filled-rectangle" "rectangle") (list select-tile initial-pt (evt-clamp canvas mouse-event))]
                           [("circle" "weird-circle" "weird-rectangle" "weird-star" "diamond") (list select-tile (evt-clamp canvas mouse-event) radius)]))
                       
                       (define (handle-left-up mouse-event)
                         (set! placing #f)
                         (send canvas unselect-all)
                         (apply tracer (build-tracer-args mouse-event))
                         
                         (set! history (history-add-action history
                                                           (action 'compound
                                                                   (map (lambda (t) (action 'atomic (list t)))
                                                                        tiles-drawn))))
                         (set! tiles-drawn null)
                         (send canvas scene-draw))
                       
                       (define (handle-select mouse-event)
                         (if (false? (member shape (list "line" "filled-rectangle" "rectangle")))
                             (begin (send canvas unselect-all)
                                    (apply (find-selector shape) (build-selector-args mouse-event)))
                             (when (and placing
                                        (not (equal? (pt (send mouse-event get-x) (send mouse-event get-y))
                                                     last-mouse-pt)))
                               (send canvas unselect-all)
                               (apply (find-selector shape) (build-selector-args mouse-event))
                               (set! last-mouse-pt (pt (send mouse-event get-x) (send mouse-event get-y))))))   
         
                       (define (find-tracer s)
                         (case s
                           [("line") trace-line]
                           [("circle") trace-circle]
                           [("rectangle") trace-unfilled-rectangle]
                           [("filled-rectangle") trace-filled-rectangle]
                           [("weird-circle") trace-weird-circle]
                           [("weird-rectangle") trace-weird-rectangle]
                           [("weird-star") trace-weird-star]
                           [("diamond") trace-diamond]))
                       
                       (define (find-selector s)
                         (case s
                           [("line") trace-line]
                           [("circle") trace-circle]
                           [("rectangle") trace-unfilled-rectangle]
                           [("filled-rectangle") trace-unfilled-rectangle]
                           [("weird-circle") trace-weird-circle]
                           [("weird-rectangle") trace-weird-rectangle]
                           [("weird-star") trace-weird-star]
                           [("diamond") trace-diamond]))
                       
                       (define/public (set-shape s) (set! shape s) (set! tracer (find-tracer s)))
                       (define/public (get-shape) shape)
                       (define/public (get-shapes) shapes)
                       (define/public (set-tile t) (set! tile t))
                       (define/public (set-history h) (set! history h))
                       (define/public (get-history) (let ([h history]) (begin (set! history null) h)))
                       (define/public (get-name) "Shape")
                       (define/public (set-scene c) (set! scene c))
                       (define/public (set-canvas c) (set! canvas c))
                       (define/public (set-radius n) (set! radius n))
                       
                       (define/public (handle mouse-event)
                         (case (send mouse-event get-event-type)
                           [(left-down) (handle-left-down mouse-event)]
                           [(left-up) (handle-left-up mouse-event)]
                           [else (handle-select mouse-event)]))))
; this sucks - the only method used is get-name
(define select-brush% (class* object% (brush-with-selection-interface)
                        (init-field canvas scene)
                        
                        (field [history null]
                               [tile empty-tile]
                               [selected-points null] 
                               [width (send scene get-width)]
                               [height (send scene get-height)]
                               [selected-pt null])
                        
                        (super-new)
                        
                        (define/public (set-history h) (set! history h))
                        (define/public (get-history) (let ([h history]) (begin (set! history null) h)))
                        (define/public (get-name) "Select")
                        (define/public (set-scene c) (set! scene c))
                        (define/public (set-tile t) (set! tile t))
                        (define/public (set-canvas c) (set! canvas c))
                        (define/public (get-selected-points) selected-points)
                        (define/public (get-selected-pt) selected-pt)
                        (begin-encourage-inline (define (good-xy? x y)
                                                  (and (unsafe-fx>= y 0) (unsafe-fx>= x 0) (unsafe-fx< x width) (unsafe-fx< y height))))

                        (define/public (handle mouse-event)
                          #f)))