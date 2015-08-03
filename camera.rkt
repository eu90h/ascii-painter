#lang racket 

(provide camera%)

(require "point.rkt" "interval.rkt")

; a camera object represents the location of a camera within a scene being painted on a given canvas.
(define camera% (class object%
  ; Pt Interval Interval Integer Integer
	(init-field pos scene-x-interval scene-y-interval canvas-width canvas-height)

	(super-new)

  ; Pt -> Boolean
  ; returns true if the rectangle formed by the given point p and 
  ; the point p + (canvas-width, canvas-height) lies within a given scene
	(define (valid-pos? p)
    	(and (pt-within-bounds? p scene-x-interval scene-y-interval) 
    		(pt-within-bounds? (pt-add p (pt canvas-width canvas-height)) scene-x-interval scene-y-interval)))

  ; Pt -> Pt
  ; Adds p to the current camera position if p + camera-position is a valid-pos?
	(define (safe-add p)
  	(if (valid-pos? (pt-add pos p)) 
      (pt-add pos p)
      pos))

  ; Integer Integer -> Void
  ; moves the camera by a given displacement
	(define/public (move dx dy)
		(set! pos (safe-add (pt dx dy))) pos)

  ; Integer Integer -> Void
  ; updates the scene boundaries
  (define/public (set-scene-intervals x y) (set! scene-x-interval x) (set! scene-y-interval y))

  ; Integer Integer -> Void
  (define/public (set-position x y) (if (valid-pos? (pt x y)) (begin (set! pos (pt x y)) pos) pos))

  ; Void -> Pt
  (define/public (get-position) pos)))

(module+ test
  (require rackunit "util.rkt")

  (define (get-arbitrary-pt (min-x 0) (min-y 0)) 
    (if (and (zero? min-x) (zero? min-y))
      (pt (random-integer 0 4096) (random-integer 0 4096))
      (pt (random-integer 0 min-x) (random-integer 0 min-y))))

  (define scene-width (random-integer 1 4096))
  (define scene-height (random-integer 1 4096))
  (define scene-x-interval (interval 0 scene-width))
  (define scene-y-interval (interval 0 scene-height))
  (define canvas-width (random-integer 0 100))
  (define canvas-height (random-integer 0 100))
  (define pos (get-arbitrary-pt scene-width scene-height))
  
  (define c (new camera% [pos pos]
    [scene-x-interval scene-x-interval] [scene-y-interval scene-y-interval]
    [canvas-width canvas-width] [canvas-height canvas-height]))

  (let move-camera-around ([n 100])
    (unless (<= n 0)
      (let ([dx (random-integer -9999 99999)] [dy (random-integer -99999 999999)])
        (move-camera-around (sub1 n)))))

  (let ([old-pos (send c get-position)])
    (check-equal? (send c set-position -1 -1) old-pos))

  (let ([old-pos (send c get-position)])
    (check-equal? (send c set-position (interval-right scene-x-interval) 0) old-pos)
    (check-equal? (send c set-position (interval-right scene-x-interval) (interval-left scene-y-interval))
                  old-pos)))


