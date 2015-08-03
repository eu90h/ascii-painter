#lang racket
(require racket/unsafe/ops)
(provide (struct-out pt) 
	(contract-out 
		[pt-add (-> (struct/c pt integer? integer?) (struct/c pt integer? integer?) (struct/c pt integer? integer?))]
		[pt-sub (-> (struct/c pt integer? integer?) (struct/c pt integer? integer?) (struct/c pt integer? integer?))]
		[pt-mag (-> (struct/c pt integer? integer?) integer?)]))

(struct pt (x y) #:transparent)

; Pt Pt -> Pt
; Adds two points
(define (pt-add p q)
	(pt (unsafe-fx+ (pt-x p) (pt-x q))
		(unsafe-fx+ (pt-y p) (pt-y q))))

; Pt Pt -> Pt
; Subtracts two points
(define (pt-sub p q)
	(pt (unsafe-fx- (pt-x p) (pt-x q))
		(unsafe-fx- (pt-y p) (pt-y q))))

(define (unsafe-fxsqr a) (unsafe-fx* a a))

; Pt -> Number
; Returns the distance of the point from the origin (0,0)
(define (pt-mag p)
	(unsafe-fx+ (unsafe-fxsqr (pt-x p)) (unsafe-fxsqr (pt-y p))))