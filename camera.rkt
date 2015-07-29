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
  	(if (valid-pos? (pt-add pos p)) (pt-add pos p) pos))

  ; Integer Integer -> Void
  ; moves the camera by a given displacement
	(define/public (move dx dy)
		(set! pos (safe-add (pt dx dy))))

  ; Integer Integer -> Void
  ; updates the scene boundaries
  (define/public (set-scene-intervals x y) (set! scene-x-interval x) (set! scene-y-interval y))

  ; Integer Integer -> Void
  (define/public (set-position x y) (set! pos (pt x y)))

  ; Void -> Pt
  (define/public (get-position) pos)))