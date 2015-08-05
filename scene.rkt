#lang racket/gui

(define (color? c) (and (object? c) (is-a? c color%)))

(provide (contract-out
  [struct tile ([symbol char?] [fg color?] [bg color?] [descr string?])])
  unsafe-tile-symbol
  unsafe-tile-fg
  unsafe-tile-bg
  unsafe-tile-descr
  scene%
  empty-tile
  selection-tile)

(require racket/serialize racket/unsafe/ops)

; a tile is a Char, Color, Color, String
; the atomic unit of which scenes are composed is the tile
(serializable-struct tile (symbol fg bg descr))
(define tile/c (struct/c tile char? color? color? string?))
(define (unsafe-tile-symbol t) (unsafe-struct-ref t 0))
(define (unsafe-tile-fg t) (unsafe-struct-ref t 1))
(define (unsafe-tile-bg t) (unsafe-struct-ref t 2))
(define (unsafe-tile-descr t) (unsafe-struct-ref t 3))

(define empty-tile (tile #\# (make-object color% 0 0 0 1.0) (make-object color% 0 0 0 1.0) "empty"))
(define selection-tile (tile #\X (make-object color% 255 255 0 1.0) (make-object color% 0 0 0 1.0) "Crosshair"))

; a scene is a collection of tiles
(define-serializable-class scene%
  object%
  
  ; Integer Integer Tile
  (init-field width height tile)
  
  (field (data (for/vector ([y height]) (for/vector ([x width]) tile)))) ; a 2d vector of tiles
  
  (super-new)
  
  ; Void -> Vector
  (define/public (get-data) data)
  
  ; Vector -> Scene
  (define/public (set-data d) (begin (set! data d) this))
  
  ; Integer Integer -> Boolean
  ; returns true if the given integers lie within the scene's width and height
  (define (good-xy? x y)
    (and (unsafe-fx>= y 0) (unsafe-fx>= x 0) (unsafe-fx< x width) (unsafe-fx< y height)))
  
  ; Integer Integer -> Tile
  ; retrieves the tile at the given location, unless the location is not good-xy?, in which case return empty-tile
  (define/public (get x y) (if (good-xy? x y) (unsafe-vector-ref (unsafe-vector-ref data y) x) empty-tile))
  
  ; Void -> Integer
  (define/public (get-width) width)
  
  ; Void -> Integer
  (define/public (get-height) height)
  
  ; Void -> Scene%
  (define/public (copy) (let ([s (make-object scene% width height tile)])
                          (for* ([x (in-range width)] [y (in-range height)])
                            (send s set x y (send this get x y))) s))
  
  ; Integer Integer Tile -> Scene
  (define/public (set x y tile) (when (good-xy? x y) (unsafe-vector-set! (unsafe-vector-ref data y) x tile) this))
  
  ; (Tile -> Tile) -> Scene
  ; given a callback that takes and returns tiles, applies it to all tiles in the scene and updates them with the
  ; output of the callback
  (define/public (process-tiles fn) (for* ([x (in-range width)] [y (in-range height)])
                                      (send this set x y (fn (send this get x y))) this)))

