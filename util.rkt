#lang racket/gui

(provide colors random-element get-random-color get-random-symbol rect-fill trace-line evt-clamp)

(require "symbol.rkt" "scene.rkt" "point.rkt")

(define (rect-fill scene p q tile)
	(when (> (pt-mag p) (pt-mag q))
		(rect-fill q p tile))

	(define min-x (min (pt-x p) (pt-x q)))
	(define min-y (min (pt-y p) (pt-y q)))

	(define max-x (max (pt-x p) (pt-x q)))
	(define max-y (max (pt-y p) (pt-y q)))

	(for* ([x (in-range min-x max-x)]
		   [y (in-range min-y max-y)])
		(send scene set x y tile)))

(define colors (send the-color-database get-names))

(define (random-element list) (list-ref list (random (length list))))

(define (get-random-color) (send the-color-database find-color (random-element colors)))

(define (get-random-symbol) (string->symbol (random-element cp437-strings)))

(define (evt-clamp canvas evt)
	(send canvas clamp (send evt get-x) (send evt get-y)))
;
(define (trace-line callback p q)
  (define dx (abs (- (pt-x q) (pt-x p))))
  (define dy (abs (- (pt-y q) (pt-y p))))
  (define (sign x) (if (< x 0.0) -1.0 1.0))
  (define (next x) (if (>= x (pt-x q)) (pt-x q) (+ 1.0 x)))
  (define the-pts null)
  (define (apply-callback x y) 
    (when (eq? #f (member (list x y) the-pts))
      (callback x y)
      (set! the-pts (append the-pts (list (list x y))))))
 (define (trace)
     (let ([error 0.0] [delta-error (if (zero? dx) 0.0 (abs (/ dy dx)))] [y-step (sign (- (pt-y q) (pt-y p)))])
      (define (loop x y0 e0)
      (unless (= (pt-x q) x)
        (apply-callback (inexact->exact x) (inexact->exact y0))
        (apply loop (let f ([y y0] [e (+ e0 delta-error)])
                      (if (>= e 0.5)
                        (begin (apply-callback (inexact->exact x) (inexact->exact y))
                               (f (+ y y-step) (- e 1.0)))
                        (list (next x) y e))))))
    (loop (pt-x p) (pt-y p) error)
    (apply-callback (pt-x q) (pt-y q))))
  (cond [(> (pt-x p) (pt-x q)) (trace-line callback q p)]
    [(zero? dx) (trace-column apply-callback (pt-x p) (pt-y p) (pt-y q))]
    [(zero? dy) (trace-row apply-callback (pt-y p) (pt-x p) (pt-x q))]
    [else (trace)]))

; (Integer Integer -> Void) Integer Integer Integer -> Void	
(define (trace-column callback x y0 y1)
	(let* ([diff (- y1 y0)] [fact (if (< 0 diff) -1 1)])
		(for ([i (in-range (abs diff))])
			(callback x (+ y1 (* i fact))))))

; (Integer Integer -> Void) Integer Integer Integer -> Void
(define (trace-row callback y x0 x1)
	(let* ([diff (- x1 x0)] [fact (if (< 0 diff) -1 1)])
		(for ([i (in-range (abs diff))])
			(callback (+ x1 (* i fact)) y))))
