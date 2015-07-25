
#lang racket

(require quickcheck "room.rkt")

(define arbitrary-pt (arbitrary-record pt (list pt-x pt-y) arbitrary-natural arbitrary-natural))

(define arbitrary-room 
	(arbitrary-record room (list room-lower-left-pt room-width room-height) arbitrary-pt arbitrary-natural arbitrary-natural))

; Room -> Generator
; returns a generator that produces arbitrary points lying within a given room
(define (choose-pt-from-room r)
	(let* ([ll (room-lower-left-pt r)] [x-min (pt-x ll)]
		[x-max (+ x-min (room-width r))]
		[y-min (pt-y ll)]
		[y-max (+ y-min (room-height r))])
	(generator (lambda (value rng) (pt (random-integer  x-min (add1 x-max)) 
										(random-integer  y-min (add1 y-max)))))))
; Room -> Generator
; produces a generator that returns arbitrary points outside of a given room
(define (choose-pt-from-outside-room r)
	(let* ([ll (room-lower-left-pt r)] [x-min (pt-x ll)]
		[x-max (+ x-min (room-width r))]
		[y-min (pt-y ll)]
		[y-max (+ y-min (room-height r))])
	(generator (lambda (value rng) (pt (random-integer (add1 x-max) (+ 100 x-max)) (random-integer (add1 y-max) (+ 100 y-max)))))))

; Room Pt -> Generator
; Given a room and a point inside it, return a generator that produces intersecting rooms.
(define (arbitrary-intersecting-room r pt-in-r)
	(generator (lambda (value rng) 
		(let* ([ll (pt (random-natural (add1 (pt-x (room-lower-left-pt r)))) 
						(random-natural (add1 (pt-y (room-lower-left-pt r)))))]
			[width (- (pt-x pt-in-r) (pt-x ll))]
			[height (- (pt-y pt-in-r) (pt-y ll))]) 
			(room ll width height)))))


; all this duplication could be probably be eliminated with a macro
(define room-pt-in?-returns-bool
	(property ([r arbitrary-room] [p arbitrary-pt])
		(boolean? (room-pt-in? r p))))
(displayln "room-pt-in?-returns-bool")
(quickcheck room-pt-in?-returns-bool)

(define room-pt-in?-returns-true-within
	(property ([r arbitrary-room]) (property ([p (choose-pt-from-room r)])
		(eq? #t (room-pt-in? r p)))))
(displayln "room-pt-in?-returns-true-within")
(quickcheck room-pt-in?-returns-true-within)

(define room-pt-in?-returns-false-outside
	(property ([r arbitrary-room]) (property ([p (choose-pt-from-outside-room r)])
		(eq? #f (room-pt-in? r p)))))
(displayln "room-pt-in?-returns-false-outside")
(quickcheck room-pt-in?-returns-false-outside)

(define room-intersects?-returns-bool
	(property ([r1 arbitrary-room] [r2 arbitrary-room]) 
		(boolean? (room-intersects? r1 r2))))
(displayln "room-intersects?-returns-bool")
(quickcheck room-intersects?-returns-bool)

(define a-room-intersects-itself
	(property ([r arbitrary-room])
		(eq? #t (room-intersects? r r))))
(displayln "a-room-intersects-itself")
(quickcheck a-room-intersects-itself)

(define a-room-intersects-itself-shifted-by-one
	(property ([r arbitrary-room])
		(let* ([p (room-lower-left-pt r)] [r2 (room p (room-width r) (room-height r))])
			(eq? #t (room-intersects? r r2)))))
(displayln "a-room-intersects-itself-shifted-by-one")
(quickcheck a-room-intersects-itself-shifted-by-one)

(define room-intersects?-returns-true-for-intersecting-rooms
	(property ([some-room arbitrary-room])
		(property ([pt-in-r (choose-pt-from-room some-room)])
			(property ([room-intersecting-some-room (arbitrary-intersecting-room some-room pt-in-r)])
				(eq? #t (room-intersects? some-room room-intersecting-some-room))))))
(displayln "room-intersects?-returns-true-for-intersecting-rooms")
(quickcheck room-intersects?-returns-true-for-intersecting-rooms)

(define room-ok?-returns-bool
	(property ([r arbitrary-room] [rooms (arbitrary-list arbitrary-room)])
		(boolean? (room-ok? r rooms))))
(displayln "room-ok?-returns-bool")
(quickcheck room-ok?-returns-bool)

(define room-ok?-returns-false-when-placing-already-placed-room
	(property ([some-rooms (arbitrary-list arbitrary-room)] [a-room arbitrary-room])
		(let ([the-rooms (append some-rooms (list a-room))])
			(property ([r (choose-one-of the-rooms)])
				(eq? #f (room-ok? r the-rooms))))))
(displayln "room-ok?-returns-false-when-placing-already-placed-room")
(quickcheck room-ok?-returns-false-when-placing-already-placed-room)


