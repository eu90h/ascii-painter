#lang racket/gui

(provide fill-generator% uniform-random-fill-generator% rogue-dungeon-generator%)

(require "scene.rkt" "point.rkt" "util.rkt" "room.rkt")

(define fill-generator% (class object%
  (init-field scene canvas tiles)

  (field [tile null] [new-scene null])
  
  (super-new)
  
  (define/public (get-scene) new-scene)
  
  (define dialog (new dialog% [label "Choose a tile"]))
  (define hpanel (new horizontal-panel% [parent dialog]))

  (define tile-choices (new choice% [label "Tiles"] [parent hpanel] [choices (map tile-descr tiles)]))

  (define choice->tile ((curry list-ref) tiles))

  (define (set-tile btn evt)
    (let* ([t (choice->tile (send tile-choices get-selection))]
          [s (make-object scene% (send scene get-width) (send scene get-height) t)])
      (set! new-scene s)
      (send dialog show #f)))

  (define ok-btn (new button% [label "OK"] [parent hpanel] [callback set-tile]))
 
  (define/public (process)
    (send dialog show #t))))

(define uniform-random-fill-generator% (class object%
  (init-field scene canvas tiles num-tiles)
  
  (super-new)

  (define (place-random-tile tiles)
      (send scene set
            (sub1 (random (send scene get-width)))
            (sub1 (random (send scene get-height)))
            (random-element tiles)))

  (define dialog (new dialog% [label "Choose a tile"]))
  (define hpanel (new horizontal-panel% [parent dialog]))
  (define tile-choices 
    (new list-box% [label ""] [parent hpanel] [choices (map tile-descr tiles)] 
      [style (list 'multiple)] [min-width 200] [min-height 200]))

  (define vpanel (new vertical-panel% [parent hpanel]))

  (define choice->tile ((curry list-ref) tiles))

  (define number-field (new text-field% [label "Amount to add"] [parent vpanel]))

  (define (set-tile btn evt)
    (let ([tiles (map choice->tile (send tile-choices get-selections))])
      (let loop ([n (string->number (send number-field get-value))])
        (place-random-tile tiles)
        (unless (zero? n) (loop (sub1 n)))))
    (collect-garbage)
     (send dialog show #f))
 
  (define ok-btn (new button% [label "OK"] [parent vpanel] [callback set-tile]))
 
  (define/public (process)
    (send number-field set-value "10")
    (send dialog show #t))))

(define rogue-dungeon-generator% (class object%
  (init-field scene canvas tiles rooms)

  (field [new-rooms null] [paths null] [tile null] [xmin 5] [xmax 7] [ymin 5] [ymax 7] [num-rooms 6])

  (super-new)

  (define (add-room! r) (set! new-rooms (append new-rooms (list r))))
  (define dialog (new dialog% [label "Choose a tile"]))
  (define hpanel (new horizontal-panel% [parent dialog]))
  (define tile-choices (new choice% [label "Tiles"] [parent hpanel] [choices (map tile-descr tiles)]))
  (define (room-ok? r rs) (and (andmap (lambda (v) (< v 3)) (map (lambda (v) (room-distance v r)) rs))
                              (andmap (lambda (v) (room-intersects? r v)) rs)))

  (define (set-tile btn evt)
    (send dialog show #f)

    (let place-rooms ([n num-rooms])
      (unless (<= n 0)
        (let* ([p (random-pt 0 (send scene get-width) 0 (send scene get-height))]
          [width (random-integer xmin xmax)] [height (random-integer ymin ymax)]
          [q (pt-add p (pt width height))] [walls null] [interior null])
            (trace-filled-rectangle (lambda (x y)
                (if (or (= x (pt-x q)) (= x (pt-x p)) (= y (pt-y p)) (= y (pt-y q)))
                  (set! walls (append walls (list (pt x y))))
                  (set! interior (append interior (list (pt x y)))))) p q)
            (let ([r (room walls interior)])
              (if (room-ok? r (append rooms new-rooms))
                (begin (set! new-rooms (append new-rooms (list r))) (place-rooms (sub1 n)))
                (place-rooms n))))))

    (let connect-rooms ([unconnected-rooms (append rooms new-rooms)])
      (unless (null? unconnected-rooms)
        (when (= 1 (length unconnected-rooms))
          (set! unconnected-rooms (append unconnected-rooms (random-element rooms))))
        (let* ([r1 (random-element unconnected-rooms)]
              [r2 (random-element (filter (lambda (r) (false? (equal? r r1))) unconnected-rooms))])
          (let* ([pts null] [p1 (random-wall-pt r1)] [p3 (random-wall-pt r2)] [p2 (pt (pt-x p1) (pt-y p3))])
            (trace-line (lambda (x y) (set! pts (append pts (list (pt x y))))) p1 p2)
            (trace-line (lambda (x y) (set! pts (append pts (list (pt x y))))) p2 p3)
            (set! paths (append paths (list (path r1 r2 pts)))))
          (connect-rooms (filter (lambda (r) (and (false? (equal? r r1)) (false? (equal? r r2))))
                                unconnected-rooms)))))

    (let ([t (list-ref tiles (send tile-choices get-selection))])
      (map (lambda (r) (map (lambda (p) (send scene set (pt-x p) (pt-y p) t)) (room-wall-pts r))) new-rooms)
      (map (lambda (path) (map (lambda (p) (send scene set (pt-x p) (pt-y p) t)) (path-pts path))) paths)))

  (define ok-btn (new button% [label "OK"] [parent hpanel] [callback set-tile]))
 
  (define/public (get-rooms) (append rooms new-rooms))

  (define/public (process)
    (send dialog show #t))))
                             