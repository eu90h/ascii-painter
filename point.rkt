#lang racket
(require racket/unsafe/ops)

(provide (struct-out pt) 
	(contract-out 
		[pt-add (->  pt/c pt/c pt/c)]
		[pt-sub (-> pt/c pt/c pt/c)]
		[pt-mag (-> pt/c integer?)]
		[unsafe-pt-x (-> pt/c integer?)]
		[unsafe-pt-y (-> pt/c integer?)]))

(struct pt (x y) #:transparent)
(define pt/c (struct/c pt integer? integer?))
(define (unsafe-pt-x p) (unsafe-struct-ref p 0))
(define (unsafe-pt-y p) (unsafe-struct-ref p 1))

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
; Returns the squared-distance of the point from the origin (0,0)
(define (pt-mag p)
	(unsafe-fx+ (unsafe-fxsqr (pt-x p)) (unsafe-fxsqr (pt-y p))))