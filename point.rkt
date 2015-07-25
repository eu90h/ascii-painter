#lang racket

(provide (struct-out pt) pt-add pt-mag)

(struct pt (x y) #:transparent)

(define (pt-add p q)
	(pt (+ (pt-x p) (pt-x q))
		(+ (pt-y p) (pt-y q))))

(define (pt-mag p)
	(sqrt (+ (sqr (pt-x p)) (sqr (pt-y p)))))
