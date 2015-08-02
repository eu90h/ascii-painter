#lang racket

(provide (struct-out pt) 
	(contract-out 
		[pt-add (-> (struct/c pt integer? integer?) (struct/c pt integer? integer?) (struct/c pt integer? integer?))]
		[pt-sub (-> (struct/c pt integer? integer?) (struct/c pt integer? integer?) (struct/c pt integer? integer?))]
		[pt-mag (-> (struct/c pt integer? integer?) real?)]))

(struct pt (x y))

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