#lang racket

(provide paint-brush% single-brush%  
  brush-interface brush-with-selection-interface shape-brush%)

(require "scene.rkt" "util.rkt" "point.rkt" "history.rkt" "interval.rkt")

(define brush-interface (interface () get-name set-scene set-tile set-canvas handle get-history set-history)) 
(define brush-with-selection-interface (interface (brush-interface) get-selected-points))

(define (change-tile canvas history scene x y tile) 
  (let ([p (send canvas clamp x y)])
    (send canvas draw)
    (paint-scene history scene (pt-x p) (pt-y p) tile)))

(define (xy-in-scene? scene x y) 
  (and (number-in-interval? x (interval 0 (sub1 (send scene get-width))))
    (number-in-interval? y (interval 0 (sub1 (send scene get-height))))))

(define (remove-tile canvas history scene x y)  
    (change-tile canvas history scene x y empty-tile))

(define (paint-and-append canvas history scene x y tile action-data)
  (when (xy-in-scene? scene x y)
    (values (append action-data (list (list (send scene get x y) x y))) 
            (change-tile canvas history scene x y tile))))

(define single-brush% (class* object% (brush-with-selection-interface)
	(init-field canvas scene)

  (field [history null] [tile empty-tile] [selected-points null])

	(super-new)

  (define/public (set-history h) (set! history h))
  (define/public (get-history) (let ([h history]) (begin (set! history null) h)))
	(define/public (get-name) "Single")
	(define/public (set-scene c) (set! scene c))
	(define/public (set-tile t) (set! tile t))
	(define/public (set-canvas c) (set! canvas c))
	(define/public (get-selected-points) selected-points)

	(define (good-xy? x y) 
    (and (number-in-interval? x (interval 0 (sub1 (send scene get-width))))
      (number-in-interval? y (interval 0 (sub1 (send scene get-height))))))

  (define (change-tile x y)
    (let ([p (send canvas clamp x y)])
        (when (good-xy? (pt-x p) (pt-y p))
          (set! history (paint-scene history scene (pt-x p) (pt-y p) tile)))))

  (define (remove-tile x y) 
    (let ([p (send canvas clamp x y)]) 
      (set! history (paint-scene history scene (pt-x p) (pt-y p) empty-tile)))
    (send canvas draw))
	
  (define/public (handle mouse-event)
    (when (eq? 'right-up (send mouse-event get-event-type))
			(remove-tile (send mouse-event get-x) (send mouse-event get-y)))
		(when (eq? 'left-up (send mouse-event get-event-type))
			(change-tile (send mouse-event get-x) (send mouse-event get-y))))))

(define paint-brush% (class* object% (brush-interface)
	(init-field canvas scene)

  (field [history null] [tile empty-tile] [drawing #f] [removing #f])

  (super-new)

  (define/public (set-history h) (set! history h))
  (define/public (get-history) (let ([h history]) (begin (set! history null) h)))
  (define/public (get-name) "Paint")
  (define/public (set-scene c) (set! scene c))
  (define/public (set-tile t) (set! tile t))
  (define/public (set-canvas c) (set! canvas c))

   (define (good-xy? x y) 
    (and (number-in-interval? x (interval 0 (sub1 (send scene get-width))))
      (number-in-interval? y (interval 0 (sub1 (send scene get-height))))))

  (define (remove-tile x y) 
    (let ([p (send canvas clamp x y)])
      (set! history (paint-scene history scene (pt-x p) (pt-y p) empty-tile))))

  (define (change-tile x y)
    (let ([p (send canvas clamp x y)])
        (when (good-xy? (pt-x p) (pt-y p))
          (set! history (paint-scene history scene (pt-x p) (pt-y p) tile)))))

  (define/public (handle mouse-event)
    (when drawing (change-tile (send mouse-event get-x) (send mouse-event get-y)))
    (when removing (remove-tile (send mouse-event get-x) (send mouse-event get-y)))
    (case (send mouse-event get-event-type)
      [(left-down) (set! drawing #t) (set! removing #f)]
      [(left-up) (set! drawing #f)]
      [(right-down) (set! drawing #f) (set! removing #t)]
      [(right-up) (set! removing #f)]))))

(define shape-brush% (class* object% (brush-with-selection-interface)
  (init-field canvas scene)

  (field [tile empty-tile] [history null] (selected-points null) [placing #f] [initial-pt (pt 0 0)]
    [tiles-drawn null] [radius 4] [tracer trace-line] [shape "line"] 
    [shapes (list "line" "filled-rectangle" "circle" "weird-circle" "weird-rectangle" "weird-star" "diamond")])

  (super-new)

  (define (good-xy? x y) 
    (and (number-in-interval? x 
      (interval 0 (send scene get-width))) (number-in-interval? y (interval 0 (send scene get-height)))))

  (define (select-tile x y) 
      (set! selected-points (append selected-points (list (pt x y)))))
  
  (define (set-and-accumulate x y)
    (when (good-xy? x y)
      (set! tiles-drawn (append tiles-drawn (list (list (send scene get x y) x y))))
      (send scene set x y tile)))

  (define (handle-left-down mouse-event)
    (set! placing #t)
    (set! initial-pt (evt-clamp canvas mouse-event)))

  (define (build-tracer-args mouse-event)
    (case shape
      [("line" "filled-rectangle") (list initial-pt (evt-clamp canvas mouse-event))]
      [("circle" "weird-circle" "weird-rectangle" "weird-star" "diamond") (list (evt-clamp canvas mouse-event) radius)]))

  (define (handle-left-up mouse-event)
    (set! placing #f)
    (apply ((curry tracer) set-and-accumulate) (build-tracer-args mouse-event))
    (send canvas scene-draw)
    (set! history (history-add-action history (action 'compound (map (lambda (t) (action 'atomic (list t))) tiles-drawn))))
    (set! tiles-drawn null))

  (define (handle-select mouse-event)
    (unless (and (list? (member shape (list "line" "filled-rectangle"))) (not placing))
      (apply tracer select-tile (build-tracer-args mouse-event))
      (send canvas draw-selected-tiles selected-points)))

  (define (find-tracer s)
    (case s
      [("line") trace-line]
      [("circle") trace-circle]
      [("filled-rectangle") trace-filled-rectangle]
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
  (define/public (get-selected-points) selected-points)

  (define/public (handle mouse-event)
    (set! selected-points null)
    (case (send mouse-event get-event-type)
      [(left-down) (handle-left-down mouse-event)]
      [(left-up) (handle-left-up mouse-event)]
      [else (handle-select mouse-event)]))))
