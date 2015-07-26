#lang racket/gui

(provide fill-generator% uniform-random-fill-generator% rectangle-generator% room-connector-generator%)

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
                                         
                                       ;  (define/public (get-scene) new-scene)
                          
                                         (define dialog (new dialog% [label "Choose a tile"]))
                                         (define hpanel (new horizontal-panel% [parent dialog]))
                                         (define tile-choices (new list-box% [label ""] [parent hpanel] [choices (map tile-descr tiles)] [style (list 'multiple)]
                                                                   [min-width 200] [min-height 200]))

                                         (define choice->tile ((curry list-ref) tiles))
                                         (define number-field (new text-field% [label "Amount to add"] [parent hpanel]))
                                         (define (set-tile btn evt)
                                           (let ([s (send scene copy)] [tiles (map choice->tile (send tile-choices get-selections))])
                                             (let loop ([n (string->number (send number-field get-value))])
                                               (place-random-tile tiles)
                                               (unless (zero? n) (loop (sub1 n)))))
                                           (send dialog show #f))
                                         
                                         (define ok-btn (new button% [label "OK"] [parent hpanel] [callback set-tile]))
                                         
                                         (define/public (process)
                                           (send number-field set-value "10")
                                           (send dialog show #t))))

  (define rectangle-generator% (class object%
                              (init-field scene canvas tiles rooms)
                              (field [tile null] [new-room null])
                              (super-new)

                             (define/public (get-room) new-room)

                              (define (add-room! r) (set! new-room r))

                              (define dialog (new dialog% [label "Choose a tile"]))
                              (define hpanel (new horizontal-panel% [parent dialog]))
                              (define tile-choices (new choice% [label "Tiles"] [parent hpanel] [choices (map tile-descr tiles)]))
                              (define choice->tile ((curry list-ref) tiles))

                              (define (set-tile btn evt)
                                (let* ([t (choice->tile (send tile-choices get-selection))]
                                  [width (+ 5 (random 5))]
                                  [height (+ 5 (random 5))]
                                  [bottom-left-pt (pt (sub1 (random (send scene get-width)))
                                                      (sub1 (random (send scene get-height))))]
                                  [r (room bottom-left-pt width height)])
                                  (if (room-ok? r rooms)
                                    (begin (for* ([x (in-range (pt-x bottom-left-pt) (+ width (pt-x bottom-left-pt)))]
                                          [y (in-range (pt-y bottom-left-pt) (+ height (pt-y bottom-left-pt)))])
                                      (send scene set x y t))
                                    (add-room! r)
                                    (send dialog show #f))
                                    (set-tile btn evt))))
                        
                              (define ok-btn (new button% [label "OK"] [parent hpanel] [callback set-tile]))
                         
                              (define/public (process)
                                (send dialog show #t))))

(define room-connector-generator% (class object%
                              (init-field scene canvas tiles rooms num-rooms-to-connect)
                              (super-new)
                              (define dialog (new dialog% [label "Choose a tile"]))
                              (define hpanel (new horizontal-panel% [parent dialog]))
                              (define tile-choices (new choice% [label "Tiles"] [parent hpanel] [choices (map tile-descr tiles)]))
                              (define choice->tile ((curry list-ref) tiles))
                              (define (set-tile btn evt)
                                (let* ([t (choice->tile (send tile-choices get-selection))])
                                  (let loop ([times (if (<= num-rooms-to-connect 1) 2 (sub1 num-rooms-to-connect))]
                                     [rand-room-1 (random-element rooms)] 
                                      [rand-room-2 (random-element rooms)])
                                
                                    (connect-rooms rand-room-1 rand-room-2 t)
                                    (unless (zero? times) 
                                      (loop (sub1 times) (random-element rooms) (random-element rooms)))))
                                  (send dialog show #f))
                        
                              (define ok-btn (new button% [label "OK"] [parent hpanel] [callback set-tile]))
                              (define (connect-rooms r s tile)
                                (if (and (= (pt-x (room-lower-left-pt r)) (pt-x (room-lower-left-pt s))) 
                                        (= (pt-y (room-lower-left-pt r)) (pt-y (room-lower-left-pt s))))
                                  (connect-rooms r s tile)
                                  (let* ([p (random-point-in-room r)]
                                       [q (random-point-in-room s)])
                                  
                                    (trace-line (lambda (x y) (send scene set x y tile)) p q))))

                              (define/public (process)
                                (send dialog show #t))))
                             
                             