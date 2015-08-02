#lang racket/gui

(define (color? c) (and (object? c) (is-a? c color%)))
(define (canvas? c) (and (object? c) (is-a? c canvas%)))
(define (event? e) (and (object? e) (is-a? e event%)))
(define (scene? e) (and (object? e) (is-a? e scene%)))
(provide (contract-out 
    [colors list?] 
    [random-element (-> list? any/c)]
    [get-random-color (-> color?)]  
    [get-random-symbol (-> char?)]
    [random-integer (-> integer? integer? integer?)]
    [get-random-pt 
        (-> integer? integer? integer? integer? 
          (struct/c pt integer? integer?))]
    [evt-clamp (-> canvas? event? (struct/c pt natural-number/c natural-number/c))]
    [trace-line (-> (-> natural-number/c natural-number/c any) 
                    (struct/c pt integer? integer?)
                    (struct/c pt integer? integer?) any)]
    [trace-filled-rectangle (-> (-> integer? integer? any) 
                    (struct/c pt integer? integer?)
                    (struct/c pt integer? integer?) any)]
    [trace-circle (-> (-> integer? integer? any) 
                    (struct/c pt integer? integer?)
                    natural-number/c any)]
    [trace-weird-circle (-> (-> integer? integer? any) 
                    (struct/c pt integer? integer?)
                    natural-number/c any)] 
    [trace-weird-star (-> (-> integer? integer? any) 
                    (struct/c pt integer? integer?)
                    natural-number/c any)]
    [trace-weird-rectangle (-> (-> integer? integer? any) 
                    (struct/c pt integer? integer?)
                    natural-number/c any)]
    [trace-diamond (-> (-> integer? integer? any) 
                    (struct/c pt integer? integer?)
                    natural-number/c any)]
    [paint-scene (-> list? scene? natural-number/c natural-number/c 
      (struct/c tile char? color? color? string?) any)])
  random-wall-pt random-interior-pt)

(require "symbol.rkt" "scene.rkt" "point.rkt" "history.rkt" "room.rkt")

(module+ test
  (require rackunit))

(define colors (send the-color-database get-names)) ; a list of color name strings

; List -> Any
; returns a random element of a list
(define (random-element list) 
  (when (list? list) 
    (if (null? list) null (list-ref list (random (length list))))))

(module+ test
  (check-eq? null (random-element null))
  (define (is-a-number-in-list? l v) (and (number? v) (list? (member v l))))
  (define list-of-nums (build-list (random 100) (lambda (x) (random 100))))

  (check-pred ((curry is-a-number-in-list?) list-of-nums) (random-element list-of-nums))
  (check-pred void? (random-element (cons 1 2))))

; Void -> Color%
; returns a random color object from the Racket color database
(define (get-random-color) (send the-color-database find-color (random-element colors)))

(module+ test
  (check-pred color? (get-random-color)))

; Void -> Char
; returns a random cp437 character
(define (get-random-symbol) (string->symbol (random-element cp437-strings)))

(module+ test
  (check-pred char? (get-random-symbol)))

; Integer Integer -> Integer
; returns a random integer between two integers (inclusive)
(define (random-integer min max) (+ min (random max)))

(module+ test
  (check-pred integer? (random-integer -99999 99999)))

; Void -> Pt
; returns a random point
(define (get-random-pt xmin xmax ymin ymax) (pt (random-integer xmin xmax) (random-integer ymin ymax)))

(define (random-wall-pt r) (random-element (room-wall-pts r)))

(define (random-interior-pt r) (random-element (room-interior-pts r)))

; Canvas% Event -> Pt
; takes a mouse event and clamps the mouse event coordinates to the given canvas
(define (evt-clamp canvas evt)
	(send canvas clamp (send evt get-x) (send evt get-y)))

; (Integer Integer -> Void) Pt Pt -> Void 
; an implementation of Bresenham's line algorithm
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
    (apply-callback (pt-x p) (pt-y p))
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
; applies a callback to every point on a vertical line
(define (trace-column callback x y0 y1)
	(for ([y (in-range (min y0 y1) (add1 (max y0 y1)))])
    (callback x y)))

; (Integer Integer -> Void) Integer Integer Integer -> Void
; applies a callback to every point on a horizontal line
(define (trace-row callback y x0 x1)
	(for ([x (in-range (min x0 x1) (add1 (max x0 x1)))])
    (callback x y)))

; (Integer Integer -> Void) Pt Integer -> Void
; applies a callback to integer points on a circle of given radius 
; (adapted from C code on the Wikipedia article for the Midpoint circle algorithm)
(define (trace-circle callback p radius)
  (define the-pts null)
  (define (apply-callback x y) 
    (when (eq? #f (member (list x y) the-pts))
      (callback x y)
      (set! the-pts (append the-pts (list (list x y))))))
  (define center-x (pt-x p))
  (define center-y (pt-y p))
  (define (loop x y decisionOver2)
    (when (>= x y)
      (apply-callback (+ x center-x) (+ y center-y))
      (apply-callback (+ y center-x) (+ x center-y))
      (apply-callback (- center-x x) (+ y center-y))
      (apply-callback (- center-x y) (+ x center-y))

      (apply-callback (+ x center-x) (- center-y y))
      (apply-callback (+ y center-x) (- center-y x))
      (apply-callback (- center-x x) (- center-y y))
      (apply-callback (- center-x y) (- center-y x))
      (if (<= decisionOver2 0)
        (loop x (add1 y) (+ decisionOver2 1 (* 2 (add1 y))))
        (loop (sub1 x) (add1 y) (+ decisionOver2 1 (* 2 (- (add1 y) (add1 x))))))))
  (loop radius 0 (- 1 radius)))

; (Integer Integer -> Void) Pt Pt
; applies a callback to all points within two pts
(define (trace-filled-rectangle callback p q)
 (when (> (pt-mag p) (pt-mag q))
   (trace-filled-rectangle callback q p))

 (define min-x (min (pt-x p) (pt-x q)))
 (define min-y (min (pt-y p) (pt-y q)))

 (define max-x (add1 (max (pt-x p) (pt-x q))))
 (define max-y (add1 (max (pt-y p) (pt-y q))))

 (for* ([x (in-range min-x max-x)]
        [y (in-range min-y max-y)])
    (callback x y)))

; (Integer Integer -> Void) Pt Integer -> Void
; applies a callback to integer points on a diamond of given radius
(define (trace-diamond callback p radius)
  (define center-x (pt-x p))
  (define center-y (pt-y p))
  (define (loop x y r error)
    (when (< x 0)
      (callback (- center-x x) (+ center-y y))
      (callback (- center-x y) (- center-y x))
      (callback (+ center-x x) (- center-y y))
      (callback (+ center-x y) (+ center-y x))
      (let ([new-r error] [new-error error] [new-x x] [new-y y])
        (when (<= new-r y) (set! new-y (add1 new-y)) (set! new-error (+ error 1 (* 2 new-y))))
        (when (or (> r x) (> error y)) (set! new-x (add1 new-x)) (set! new-error (+ new-error 1 (* 2 new-x))))
        (loop new-x new-y new-r new-error))))
  (loop (* -1 radius) 0 radius (- 2 (* 1 radius))))


; (Integer Integer -> Void) Pt Integer -> Void
; applies a callback to integer points on a "wierd" star shape of given radius
(define (trace-weird-star callback p radius)
  (define center-x (pt-x p))
  (define center-y (pt-y p))
  (define (loop x y r error)
    (when (< x 0)
      (callback (- center-x x) (+ center-y y))
      (callback (- center-x y) (- center-y x))
      (callback (+ center-x x) (- center-y y))
      (callback (+ center-x y) (+ center-y x))
      (let ([new-r error] [new-error error] [new-x x] [new-y y])
        (when (<= new-r y) (set! new-y (add1 new-y)) (set! new-error (+ error 1 (* 2 y))))
        (when (or (> r x) (> error y)) (set! new-x (add1 new-x)) (set! new-error (+ error 1 (* 2 x))))
        (loop new-x new-y new-r new-error))))
  (loop (* -1 radius) 0 radius (- 2 (* 2 radius))))

; (Integer Integer -> Void) Pt Integer -> Void
; applies a callback to integer points on a "wierd" rectangular shape of given radius
(define (trace-weird-rectangle callback p radius)
  (define center-x (pt-x p))
  (define center-y (pt-y p))
  (define (loop x y r error)
    (when (< x 0)
      (callback (- center-x x) (+ center-y y))
      (callback (- center-x y) (- center-y x))
      (callback (+ center-x x) (- center-y y))
      (callback (+ center-x y) (+ center-y x))
      (let ([new-r error] [new-error error] [new-x x] [new-y y])
        (when (<= new-r y) (set! new-y (add1 new-y)) (set! new-error (+ error 1 (* 2 new-y))))
        (when (or (> r x) (> error y)) (set! new-x (add1 new-x)) (set! new-error (+ error 1 (* 2 new-x))))
        (loop new-x new-y new-r new-error))))
  (loop (* -1 radius) 0 radius (- 2 (* 2 radius))))

; (Integer Integer -> Void) Pt Integer -> Void
; applies a callback to integer points on a "weird" circlular shape of given radius
(define (trace-weird-circle callback p radius)
  (define center-x (pt-x p))
  (define center-y (pt-y p))
  (define (loop x y r error)
    (when (< x 0)
      (callback (- center-x x) (+ center-y y))
      (callback (- center-x y) (- center-y x))
      (callback (+ center-x x) (- center-y y))
      (callback (+ center-x y) (+ center-y x))
      (let ([new-r error] [new-error error] [new-x x] [new-y y])
        (when (<= new-r y) 
          (set! new-y (add1 new-y)) (set! new-error (+ new-error 1 (* 2 new-y))))
        (when (or (> r x) (> error y)) 
          (set! new-x (add1 new-x)) (set! new-error (+ new-error 1 (* 2 new-x))))
        (loop new-x new-y new-r new-error))))
  (loop (* -1 radius) 0 radius (- 2 (* 2 radius))))

(define HISTORY-MAX 2000) ; max number of items in history
(define HISTORY-DROP 500) ; how many actions to remove from the history after going over HISTORY-MAX

; History Scene Natural Natural Tile -> History
; paint a scene's tile at the given coordinates and then returns a history with the paint action added
(define (paint-scene history scene x y tile)
  (when (>= (length history) HISTORY-MAX) (drop history HISTORY-DROP))
  (define new-history (history-add-action history (action 'atomic (list (list (send scene get x y) x y)))))
    (send scene set x y tile)
    new-history)
