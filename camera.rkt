#lang racket 

(provide camera%)

(require "point.rkt" "interval.rkt")
(define camera% (class object%
	(init-field pos scene-x-interval scene-y-interval canvas-width canvas-height)

	(super-new)

  	(define (valid-pos? p)
      	(and (pt-within-bounds? p scene-x-interval scene-y-interval) 
      		(pt-within-bounds? (pt-add p (pt canvas-width canvas-height)) scene-x-interval scene-y-interval)))

  	(define (safe-add p)
    	(if (valid-pos? (pt-add pos p)) (pt-add pos p) pos))

  	(define/public (set-scene-intervals x y) (set! scene-x-interval x) (set! scene-y-interval y))
    (define/public (set-position x y) (set! pos (pt x y)))
  	(define/public (get-position) pos)

  	(define/public (move dx dy)
  		(set! pos (safe-add (pt dx dy))))))