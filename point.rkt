#lang racket

(provide (struct-out pt) pt-add pt-sub pt-mag)

(struct pt (x y) #:transparent)

; Pt Pt -> Pt
; Adds two points
(define (pt-add p q)
	(pt (+ (pt-x p) (pt-x q))
		(+ (pt-y p) (pt-y q))))

; Pt Pt -> Pt
; Subtracts two points
(define (pt-sub p q)
	(pt (- (pt-x p) (pt-x q))
		(- (pt-y p) (pt-y q))))

; Pt -> Number
; Returns the distance of the point from the origin (0,0)
(define (pt-mag p)
	(sqrt (+ (sqr (pt-x p)) (sqr (pt-y p)))))