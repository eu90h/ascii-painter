#lang racket

(provide (struct-out room) room-pt-in? room-intersects? room-ok? random-point-in-room)

(require quickcheck math/base "point.rkt")

(struct room (lower-left-pt width height) #:transparent)

(define (room-pt-in? r p)
	(let* ([ll (room-lower-left-pt r)]
		[x-min (pt-x ll)]
		[x-max (+ x-min (room-width r))]
		[y-min (pt-y ll)]
		[y-max (+ y-min (room-height r))])
		(not (or (or (< (pt-y p) y-min) (> (pt-y p) y-max)) 
			(or (< (pt-x p) x-min) (> (pt-x p) x-max))))))

(define (room-intersects? r1 r2)
	(let* ([r1-ll (room-lower-left-pt r1)]
		[r1-lr (pt-add r1-ll (pt (room-width r1) 0))]
		[r1-ul (pt-add r1-ll (pt 0 (room-height r1)))]
		[r1-ur (pt-add r1-ll (pt (room-width r1) (room-height r2)))])
		(or (room-pt-in? r2 r1-ll)
			(room-pt-in? r2 r1-lr)
			(room-pt-in? r2 r1-ul)
			(room-pt-in? r2 r1-ur))))

(define (room-ok? a-room placed-rooms)
    (let ([intersection-list (map ((curry room-intersects?) a-room) placed-rooms)])
      (not (foldl (lambda (x y) (or x y)) #f intersection-list))))

(define (random-point-in-room r)
	(let* ([ll (room-lower-left-pt r)] [x-min (pt-x ll)]
		[x-max (+ x-min (room-width r))]
		[y-min (pt-y ll)]
		[y-max (+ y-min (room-height r))])
	(pt (random-integer  x-min (add1 x-max)) (random-integer  y-min (add1 y-max)))))