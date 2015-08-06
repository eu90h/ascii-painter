#lang racket

(provide (struct-out room) (struct-out path) is-interior-pt? is-wall-pt? room-intersects? room-distance)

(require math/base "point.rkt")

(struct room (wall-pts interior-pts))
(struct path (start end pts))

(define (is-interior-pt? r p)
	(list? (member p (room-interior-pts r))))

(define (is-wall-pt? r p)
	(list? (member p (room-wall-pts r))))

(define (room-intersects? r1 r2)
	(and (andmap (lambda (v) (not (is-wall-pt? r1 v))) (room-wall-pts r2))
		 (andmap (lambda (v) (not (is-interior-pt? r1 v))) (room-interior-pts r2))))

(define (room-distance r1 r2)
	(let ([w1 (room-wall-pts r1)] [w2 (room-wall-pts r2)])
		(let loop ([pts w1] [distance 0])
			(if (null? pts) distance 
				(loop (rest pts) 
					(let ([candidate (apply min (map (lambda (p) (abs (pt-mag (pt-sub (first w1) p)))) w2))])
						(if (< candidate distance) candidate distance)))))))

