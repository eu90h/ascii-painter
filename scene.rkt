#lang racket/gui

(provide scene% empty-tile (struct-out tile))

(require racket/serialize)

(serializable-struct tile (symbol fg bg descr) #:transparent) ; the atomic unit of which scenes are composed is the tile

(define empty-tile (tile #\# (make-object color% 0 0 0 1.0) (make-object color% 0 0 0 1.0) "empty"))

(define-serializable-class scene%
  object%

  (init-field width height tile)
  
  (field (data (for/vector ([y height]) (for/vector ([x width]) tile))))
  
  (super-new)
  
  (define/public (get-data) data)
  (define/public (set-data d) (begin (set! data d) this))
  
  (define/public (get x y) (if (or (too-big? x y) (too-small? x y))
                              empty-tile (vector-ref (vector-ref data y) x)))
  
  (define/public (get-width) width)
  (define/public (get-height) height)
  
  (define/public (copy) (let ([s (make-object scene% width height tile)])
                          (send s set-data data)))
                         
  (define (too-small? x y) (or (< x 0) (< y 0)))
  (define (too-big? x y) (or (>= x width) (>= y height)))
  
  (define/public (set x y tile) (unless (or (too-big? x y) (too-small? x y)) (vector-set! (vector-ref data y) x tile))))
