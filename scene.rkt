#lang racket/gui

(provide scene% empty-tile (struct-out tile) serialize-scene deserialize-scene)

(require racket/serialize "interval.rkt")

; a tile is a Char, Color, Color, String
(serializable-struct tile (symbol fg bg descr) #:transparent) ; the atomic unit of which scenes are composed is the tile

(define empty-tile (tile #\# (make-object color% 0 0 0 1.0) (make-object color% 0 0 0 1.0) "empty"))

; this is a serializable version of the color% object. since color% objects aren't serializable, this is necessary.
(define-serializable-class serial-color% object% (init-field red green blue [alpha 1.0]) 
  (define/public (get-red) red) (define/public (get-green) green) (define/public (get-blue) blue) (define/public (get-alpha) alpha) 
  (super-new))

; color% -> serial-color%
; converts a color% to a serial-color%
; this sucks but color% objects aren't serializable so we have to wrap it
(define (serialize-color c) 
 (make-object serial-color% (send c red) (send c blue) (send c green) (send c alpha)))

; color% -> serial-color%
; converts a serial-color% to a color%. this is the inverse of serialize-color.
(define (deserialize-color c) 
  (make-object color% 
    (send c get-red) 
    (send c get-blue) 
    (send c get-green) 
    (send c get-alpha)))

; tile -> tile
; converts the color% objects inside the tile into serial-color% objects
(define (serialize-tile t) 
  (tile (tile-symbol t) (serialize-color (tile-fg t)) (serialize-color (tile-bg t)) (tile-descr t)))

; tile -> tile
; converts the serial-color% objects inside the tile into color% objects. this the inverse of serialize-tile
(define (deserialize-tile t) 
  (tile (tile-symbol t) (deserialize-color (tile-fg t)) (deserialize-color (tile-bg t)) (tile-descr t)))

; scene -> serialized-object
; serializes all tiles in a scene
(define (serialize-scene s)
 (send s process-tiles serialize-tile) (serialize (send s get-data)))

; scene -> scene
; deserializes all tiles in a scene
(define (deserialize-scene s)
  (let* ([data (deserialize s)] [height (vector-length data)] [width (vector-length (vector-ref data 0))] [new-scene (make-object scene% width height empty-tile)])
    (send new-scene set-data data)
    (send new-scene process-tiles deserialize-tile)
    new-scene))

; a scene is a collection of tiles
(define-serializable-class scene%
  object%

  ; Integer Integer Tile
  (init-field width height tile)
  
  (field (data (for/vector ([y height]) (for/vector ([x width]) tile)))) ; a 2d vector of tiles

  ; these intervals represent the boundaries of the scene
  (field [x-interval (interval 0 (sub1 width))] [y-interval (interval 0 (sub1 height))])

  (super-new)
  
  ; Void -> Vector
  (define/public (get-data) data)

  ; Vector -> Scene
  (define/public (set-data d) (begin (set! data d) this))

  ; Integer Integer -> Boolean
  ; returns true if the given integers lie within the scene's width and height
  (define (good-xy? x y) 
    (and (number-in-interval? x x-interval) (number-in-interval? y y-interval)))

  ; Integer Integer -> Tile
  ; retrieves the tile at the given location, unless the location is not good-xy?, in which case return empty-tile
  (define/public (get x y) (if (good-xy? x y) (vector-ref (vector-ref data y) x) empty-tile))
  
  ; Void -> Integer
  (define/public (get-width) width)

  ; Void -> Integer
  (define/public (get-height) height)
  
  ; Void -> Scene%
  (define/public (copy) (let ([s (make-object scene% width height tile)])
                          (for* ([x (in-range width)] [y (in-range height)])
                              (send s set x y (send this get x y))) s))
                         
  
  ; Integer Integer Tile -> Scene
  (define/public (set x y tile) (when (good-xy? x y) (vector-set! (vector-ref data y) x tile) this))

  ; (Tile -> Tile) -> Scene
  ; given a callback that takes and returns tiles, applies it to all tiles in the scene and updates them with the
  ; output of the callback
  (define/public (process-tiles fn) (for* ([x (in-range width)] [y (in-range height)])
    (send this set x y (fn (send this get x y))) this)))

