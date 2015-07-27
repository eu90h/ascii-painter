#lang racket/gui

(provide scene% empty-tile (struct-out tile) serialize-scene deserialize-scene)

(require racket/serialize)

(serializable-struct tile (symbol fg bg descr) #:transparent) ; the atomic unit of which scenes are composed is the tile

(define empty-tile (tile #\# (make-object color% 0 0 0 1.0) (make-object color% 0 0 0 1.0) "empty"))

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

(define-serializable-class scene%
  object%

  (init-field width height tile)
  
  (field (data (for/vector ([y height]) (for/vector ([x width]) tile))))
  
  (super-new)
  
  (define/public (get-data) data)
  (define/public (set-data d) (begin (set! data d) this))

   (define (too-small? x y) (or (< x 0) (< y 0)))
  (define (too-big? x y) (or (>= x width) (>= y height)))

  (define/public (get x y) (if (or (too-big? x y) (too-small? x y))
                              empty-tile (vector-ref (vector-ref data y) x)))
  
  (define/public (get-width) width)
  (define/public (get-height) height)
  
  (define/public (copy) (let ([s (make-object scene% width height tile)])
                          (for* ([x (in-range width)] [y (in-range height)])
                              (send s set x y (send this get x y))) s))
                         
  
  (define/public (set x y tile) (unless (or (too-big? x y) (too-small? x y)) (vector-set! (vector-ref data y) x tile) this))
  (define/public (process-tiles fn) (for* ([x (in-range width)] [y (in-range height)])
    (send this set x y (fn (send this get x y))) this)))

