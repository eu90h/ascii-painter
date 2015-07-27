#lang racket
(provide paint-brush% single-brush% line-brush%)

(require "scene.rkt" "util.rkt" "point.rkt")

(define single-brush% (class object%
	(init-field canvas scene)

	(field (tile empty-tile))

	(super-new)

	(define/public (get-name) "Single")

	(define/public (set-scene c) (set! scene c))

	(define/public (set-tile t) (set! tile t))

	(define/public (set-canvas c) (set! canvas c))

	(define/public (get-selected-points) null)

	(define (change-tile x y) 
          (let ([p (send canvas clamp x y)]) (send scene set (pt-x p) (pt-y p) tile))
          (send canvas draw))

                        (define (remove-tile x y) 
       (let ([p (send canvas clamp x y)]) (send scene set (pt-x p) (pt-y p) empty-tile))
      (send canvas draw))
	(define/public (handle mouse-event)
          (when (eq? 'right-up (send mouse-event get-event-type))
			(remove-tile (send mouse-event get-x) (send mouse-event get-y)))
		(when (eq? 'left-up (send mouse-event get-event-type))
			(change-tile (send mouse-event get-x) (send mouse-event get-y))))))

(define paint-brush% (class object%
	(init-field canvas scene)

	(field (tile empty-tile))
                       (field (drawing? #f))
                       (field (removing #f))
                       (super-new)

                       (define/public (get-name) "Paint")
                       
                       (define/public (set-scene c) (set! scene c))
                       
                       (define/public (set-tile t) (set! tile t))
                       
                       (define/public (set-canvas c) (set! canvas c))
                       
                       (define/public (get-selected-points) null)
                       (define/public (get-selected-tiles) null)
                       
                       (define (remove-tile x y) 
                         (let ([p (send canvas clamp x y)]) (send scene set (pt-x p) (pt-y p) empty-tile)))
                       
                       (define (change-tile x y)
                         (let ([p (send canvas clamp x y)]) (send scene set (pt-x p) (pt-y p) tile)))
                       
                       (define true? (compose not false?))
                       (define/public (handle mouse-event)
                         (when (true? drawing?) (change-tile (send mouse-event get-x) (send mouse-event get-y))
                           (send canvas draw))
                         (when (true? removing)  (remove-tile (send mouse-event get-x) (send mouse-event get-y))
                           (send canvas draw))

                         (case (send mouse-event get-event-type)
                           [(left-down) (set! drawing? #t) (set! removing #f)]
                           [(left-up) (set! drawing? #f) (send canvas draw)]
                           [(right-down) (set! drawing? #f) (set! removing #t)]
                           [(right-up) (set! removing #f) (send canvas draw)]))))

(define line-brush% (class object%
                      (init-field canvas scene)
                      
                      (field (tile empty-tile))
                      
                      (field (selected-points null))
                      (field [drawing #f])
                      (field (origin-pt (pt 0 0)))
                      
                      (super-new)
                      
                      (define/public (get-name) "Line")
                      
                      (define/public (set-scene c) (set! scene c))
                      
                      (define/public (set-tile t) (set! tile t))
                      
                      (define/public (set-canvas c) (set! canvas c))
                      
                      (define/public (get-selected-points) selected-points)
                      
                      (define (change-tile x y) (send scene set x y tile))
                      
                      (define (pt-change-tile p) (send scene set (pt-x p) (pt-y p) tile) (send canvas draw))
                      
                      (define (get-tile p) (send scene get (pt-x p) (pt-y p)))
                      
                      (define (change-tiles pts) (map (lambda (p) (change-tile (pt-x p) (pt-y p))) pts))

                      (define (select-tile x y) 
                          (set! selected-points (append selected-points (list (pt x y)))))

                      (define/public (handle mouse-event)
                        (set! selected-points null)
                        (case (send mouse-event get-event-type)
                          [(left-down) (set! drawing #t) (set! origin-pt (evt-clamp canvas mouse-event)) 
                                       (pt-change-tile origin-pt)]
                          [(left-up) (set! drawing #f) 
                          (trace-line change-tile origin-pt (evt-clamp canvas mouse-event)) (send canvas draw)]
                          [else 
                          (when drawing
                            (trace-line select-tile origin-pt 
                              (evt-clamp canvas mouse-event))
                            (send canvas draw-selected-tiles))
                            (void)]))))

