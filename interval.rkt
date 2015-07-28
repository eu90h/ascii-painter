#lang racket

(provide (struct-out interval) number-in-interval? pt-within-bounds?)

(require "point.rkt")

(struct interval (left right) #:transparent)

; Number Interval -> Boolean
; Tests whether a number is in an interval
(define (number-in-interval? x i)
	(and (<= (interval-left i) x) (>= (interval-right i) x)))

; Point Interval Interval -> Boolean
; Given a 2d point, tests whether its coordinates are in the given intervals
(define (pt-within-bounds? p x-interval y-interval)
	(and (number-in-interval? (pt-x p) x-interval) (number-in-interval? (pt-y p) y-interval)))