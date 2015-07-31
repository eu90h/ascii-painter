#lang racket/gui

(provide fill-generator% uniform-random-fill-generator% rogue-dungeon-generator%)

(require "scene.rkt" "point.rkt" "util.rkt" "room.rkt")

;; TODO: tons of duplication here with the tile choosing gui - fix this!
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
  (define tile-choices (new list-box% [label ""] [parent hpanel] [choices (map tile-descr tiles)] [style (list 'multiple)]
                           [min-width 200] [min-height 200]))

  (define vpanel (new vertical-panel% [parent hpanel]))

  (define choice->tile ((curry list-ref) tiles))

  (define number-field (new text-field% [label "Amount to add"] [parent vpanel]))

  (define (set-tile btn evt)
    (let ([s (send scene copy)] [tiles (map choice->tile (send tile-choices get-selections))])
      (let loop ([n (string->number (send number-field get-value))])
        (place-random-tile tiles)
        (unless (zero? n) (loop (sub1 n)))))
     (send dialog show #f))
 
  (define ok-btn (new button% [label "OK"] [parent vpanel] [callback set-tile]))
 
  (define/public (process)
    (send number-field set-value "10")
    (send dialog show #t))))

(define rogue-dungeon-generator% (class object%
  (init-field scene canvas tiles rooms)

  (field [tile null] [new-room null] [xmin 5] [xmax 7] [ymin 5] [ymax 7] [num-rooms 6])

  (super-new)

  (define/public (get-room) new-room)

  (define (add-room! r) (set! new-room r))

  (define dialog (new dialog% [label "Choose a tile"]))
  (define hpanel (new horizontal-panel% [parent dialog]))
  (define tile-choices (new choice% [label "Tiles"] [parent hpanel] [choices (map tile-descr tiles)]))

  (define choice->tile ((curry list-ref) tiles))

  (define (set-tile btn evt)
    (define (place-rooms n)
      (unless (<= n 0)
        (let* ([t (choice->tile (send tile-choices get-selection))]
          [p (get-random-pt 0 (send scene get-width) 0 (send scene get-height))]
          [width (get-random-integer xmin xmax)] [height (get-random-integer ymin ymax)]
          [r (room p width height)])
            (if (room-ok? r rooms)
              (begin (trace-filled-rectangle (lambda (x y) (begin (send scene set x y t) 
                                                      (add-room! r)
                                                      (send dialog show #f)))
                                      p (pt-add p (pt width height)))
                    (place-rooms (sub1 n)))
              (set-tile btn evt)))))
    (place-rooms num-rooms))

  (define ok-btn (new button% [label "OK"] [parent hpanel] [callback set-tile]))

  (define/public (process)
    (send dialog show #t))))
                             