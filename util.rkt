#lang racket/gui
(require racket/unsafe/ops)
(define (color? c) (and (object? c) (is-a? c color%)))
(define (canvas? c) (and (object? c) (is-a? c canvas%)))
(define (event? e) (and (object? e) (is-a? e event%)))
(define (scene? e) (and (object? e) (is-a? e scene%)))
(define tile/c (struct/c tile char? color? color? string?))
(define pt/c (struct/c pt integer? integer?))

(provide
 (contract-out 
  [colors list?] 
  [random-element (-> list? any/c)]
  [random-color (-> color?)]  
  [random-integer (-> integer? integer? integer?)]
  [random-pt (-> integer? integer? integer? integer? pt/c)]
  [evt-clamp (-> canvas? event? (struct/c pt natural-number/c natural-number/c))]
  [trace-line (-> (-> natural-number/c natural-number/c any) pt/c pt/c any)]
  [trace-filled-rectangle (-> (-> integer? integer? any) pt/c pt/c any)]
  [trace-unfilled-rectangle (-> (-> integer? integer? any) pt/c pt/c any)]
  [trace-circle (-> (-> integer? integer? any) pt/c natural-number/c any)]
  [trace-weird-circle (-> (-> integer? integer? any) pt/c natural-number/c any)] 
  [trace-weird-star (-> (-> integer? integer? any) pt/c natural-number/c any)]
  [trace-weird-rectangle (-> (-> integer? integer? any) pt/c natural-number/c any)]
  [trace-diamond (-> (-> integer? integer? any) pt/c natural-number/c any)]
  [paint-scene (-> list? scene? canvas? natural-number/c natural-number/c tile/c any)])
 random-wall-pt random-interior-pt
 color-equal? tile-equal?)

(require "scene.rkt" "point.rkt" "history.rkt" "room.rkt")

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
(define (random-color) (send the-color-database find-color (random-element colors)))

(module+ test
  (check-pred color? (random-color)))

; Integer Integer -> Integer
; returns a random integer between two integers (inclusive)
(define (random-integer min max) (+ min (random max)))

(module+ test
  (check-pred integer? (random-integer -99999 99999)))

; Void -> Pt
; returns a random point
(define (random-pt xmin xmax ymin ymax) (pt (random-integer xmin xmax) (random-integer ymin ymax)))

(define (random-wall-pt r) (random-element (room-wall-pts r)))

(define (random-interior-pt r) (random-element (room-interior-pts r)))

; Canvas% Event -> Pt
; takes a mouse event and clamps the mouse event coordinates to the given canvas
(define (evt-clamp canvas evt)
  (send canvas clamp (send evt get-x) (send evt get-y)))

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

; (Integer Integer -> Void) Pt Pt -> Void
; traces the outline of a rectangle
(define (trace-unfilled-rectangle callback p q)
  (define the-pts null)
  (define (apply-callback x y) 
    (when (eq? #f (member (list x y) the-pts))
      (callback x y)
      (set! the-pts (append the-pts (list (list x y))))))
  (define min-x (unsafe-fxmin (unsafe-pt-x p) (unsafe-pt-x q)))
  (define min-y (unsafe-fxmin (unsafe-pt-y p) (unsafe-pt-y q)))
  
  (define max-x (unsafe-fx+ 1 (unsafe-fxmax (unsafe-pt-x p) (unsafe-pt-x q))))
  (define max-y (unsafe-fx+ 1 (unsafe-fxmax (unsafe-pt-y p) (unsafe-pt-y q))))
  
  (trace-column apply-callback min-x min-y max-y)
  (trace-column apply-callback max-x min-y max-y)
  
  (trace-row apply-callback min-y min-x max-x)
  (trace-row apply-callback max-y min-x max-x))

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

; (Integer Integer -> Void) Pt Integer -> Void
; applies a callback to integer points on a circle of given radius 
; (adapted from C code on the Wikipedia article for the Midpoint circle algorithm)
(define (trace-circle callback p radius)
  (define center-x (unsafe-pt-x p))
  (define center-y (unsafe-pt-y p))
  (define the-pts null)
  (define (apply-callback x y) 
    (when (eq? #f (member (list x y) the-pts))
      (callback x y)
      (set! the-pts (append the-pts (list (list x y))))))
  (define (loop x y decisionOver2)
    (when (unsafe-fx>= x y)
      
      (apply-callback (unsafe-fx+ x center-x) (unsafe-fx+ y center-y))
      (apply-callback (unsafe-fx+ y center-x) (unsafe-fx+ x center-y))
      (apply-callback (unsafe-fx- center-x x) (unsafe-fx+ y center-y))
      (apply-callback (unsafe-fx- center-x y) (unsafe-fx+ x center-y))
      
      (apply-callback (unsafe-fx+ x center-x) (unsafe-fx- center-y y))
      (apply-callback (unsafe-fx+ y center-x) (unsafe-fx- center-y x))
      (apply-callback (unsafe-fx- center-x x) (unsafe-fx- center-y y))
      (apply-callback (unsafe-fx- center-x y) (unsafe-fx- center-y x))
      (if (unsafe-fx<= decisionOver2 0)
          (loop x (unsafe-fx+ y 1) (unsafe-fx+ 1 (unsafe-fx+ decisionOver2 (unsafe-fx* 2 (unsafe-fx+ 1 y)))))
          (loop (unsafe-fx- x 1) (unsafe-fx+ 1 y) (unsafe-fx+ 1 (unsafe-fx+ decisionOver2 (unsafe-fx* 2 (unsafe-fx- (unsafe-fx+ 1 y) (unsafe-fx+ 1 x)))))))))
  (loop radius 0 (unsafe-fx- 1 radius)))

; (Integer Integer -> Void) Pt Pt
; applies a callback to all points within two pts
(define (trace-filled-rectangle callback p q)
  (define min-x (unsafe-fxmin (unsafe-pt-x p) (unsafe-pt-x q)))
  (define min-y (unsafe-fxmin (unsafe-pt-y p) (unsafe-pt-y q)))
  
  (define max-x (unsafe-fx+ 1 (unsafe-fxmax (unsafe-pt-x p) (unsafe-pt-x q))))
  (define max-y (unsafe-fx+ 1 (unsafe-fxmax (unsafe-pt-y p) (unsafe-pt-y q))))
  
  (for* ([x (in-range min-x max-x)]
         [y (in-range min-y max-y)])
    (callback x y)))

; (Integer Integer -> Void) Pt Integer -> Void
; applies a callback to integer points on a diamond of given radius
(define (trace-diamond callback p radius)
  (define center-x (unsafe-pt-x p))
  (define center-y (unsafe-pt-y p))
  (define (loop x y r error)
    (when (unsafe-fx< x 0)
      (callback (unsafe-fx- center-x x) (unsafe-fx+ center-y y))
      (callback (unsafe-fx- center-x y) (unsafe-fx- center-y x))
      (callback (unsafe-fx+ center-x x) (unsafe-fx- center-y y))
      (callback (unsafe-fx+ center-x y) (unsafe-fx+ center-y x))
      (let ([new-r error] [new-error error] [new-x x] [new-y y])
        (when (unsafe-fx<= new-r y) (set! new-y (unsafe-fx+ 1 new-y)) (set! new-error (unsafe-fx+ 1 (unsafe-fx+ error (unsafe-fx* 2 new-y)))))
        (when (or (unsafe-fx> r x) (unsafe-fx> error y)) (set! new-x (unsafe-fx+ 1 new-x)) (set! new-error (unsafe-fx+ 1 (unsafe-fx+ new-error (unsafe-fx* 2 new-x)))))
        (loop new-x new-y new-r new-error))))
  (loop (unsafe-fx* -1 radius) 0 radius (unsafe-fx- 2 (unsafe-fx* 1 radius))))


; (Integer Integer -> Void) Pt Integer -> Void
; applies a callback to integer points on a "wierd" star shape of given radius
(define (trace-weird-star callback p radius)
  (define center-x (unsafe-pt-x p))
  (define center-y (unsafe-pt-y p))
  (define (loop x y r error)
    (when (unsafe-fx< x 0)
      (callback (unsafe-fx- center-x x) (unsafe-fx+ center-y y))
      (callback (unsafe-fx- center-x y) (unsafe-fx- center-y x))
      (callback (unsafe-fx+ center-x x) (unsafe-fx- center-y y))
      (callback (unsafe-fx+ center-x y) (unsafe-fx+ center-y x))
      (let ([new-r error] [new-error error] [new-x x] [new-y y])
        (when (unsafe-fx<= new-r y) (set! new-y (unsafe-fx+ 1 new-y)) (set! new-error (unsafe-fx+ 1 (unsafe-fx+ error (unsafe-fx* 2 y)))))
        (when (or (unsafe-fx> r x) (unsafe-fx> error y)) (set! new-x (unsafe-fx+ 1 new-x)) (set! new-error (unsafe-fx+ 1 (unsafe-fx+ error (unsafe-fx* 2 x)))))
        (loop new-x new-y new-r new-error))))
  (loop (unsafe-fx* -1 radius) 0 radius (unsafe-fx- 2 (unsafe-fx* 2 radius))))

; (Integer Integer -> Void) Pt Integer -> Void
; applies a callback to integer points on a "wierd" rectangular shape of given radius
(define (trace-weird-rectangle callback p radius)
  (define center-x (unsafe-pt-x p))
  (define center-y (unsafe-pt-y p))
  (define (loop x y r error)
    (when (unsafe-fx< x 0)
      (callback (unsafe-fx- center-x x) (unsafe-fx+ center-y y))
      (callback (unsafe-fx- center-x y) (unsafe-fx- center-y x))
      (callback (unsafe-fx+ center-x x) (unsafe-fx- center-y y))
      (callback (unsafe-fx+ center-x y) (unsafe-fx+ center-y x))
      (let ([new-r error] [new-error error] [new-x x] [new-y y])
        (when (unsafe-fx<= new-r y) (set! new-y (unsafe-fx+ 1 new-y)) (set! new-error (unsafe-fx+ 1 (unsafe-fx+ error (unsafe-fx* 2 new-y)))))
        (when (or (unsafe-fx> r x) (unsafe-fx> error y)) (set! new-x (unsafe-fx+ 1 new-x)) (set! new-error (unsafe-fx+ 1 (unsafe-fx+ error (unsafe-fx* 2 new-x)))))
        (loop new-x new-y new-r new-error))))
  (loop (unsafe-fx* -1 radius) 0 radius (unsafe-fx- 2 (unsafe-fx* 2 radius))))

; (Integer Integer -> Void) Pt Integer -> Void
; applies a callback to integer points on a "weird" circlular shape of given radius
(define (trace-weird-circle callback p radius)
  (define center-x (unsafe-pt-x p))
  (define center-y (unsafe-pt-y p))
  (define (loop x y r error)
    (when (unsafe-fx< x 0)
      (callback (unsafe-fx- center-x x) (unsafe-fx+ center-y y))
      (callback (unsafe-fx- center-x y) (unsafe-fx- center-y x))
      (callback (unsafe-fx+ center-x x) (unsafe-fx- center-y y))
      (callback (unsafe-fx+ center-x y) (unsafe-fx+ center-y x))
      (let ([new-r error] [new-error error] [new-x x] [new-y y])
        (when (unsafe-fx<= new-r y) 
          (set! new-y (unsafe-fx+ 1 new-y)) (set! new-error (unsafe-fx+ 1 (unsafe-fx+ new-error (unsafe-fx* 2 new-y)))))
        (when (or (unsafe-fx> r x) (unsafe-fx> error y)) 
          (set! new-x (unsafe-fx+ 1 new-x)) (set! new-error (unsafe-fx+ new-error (unsafe-fx* 2 new-x))))
        (loop new-x new-y new-r new-error))))
  (loop (unsafe-fx* -1 radius) 0 radius (unsafe-fx- 2 (unsafe-fx* 2 radius))))

(define HISTORY-MAX 2000) ; max number of items in history
(define HISTORY-DROP 1000) ; how many actions to remove from the history after going over HISTORY-MAX

; History Scene Canvas% Natural Natural Tile -> History
; paint a scene's tile at the given coordinates and then returns a history with the paint action added
(define (paint-scene history scene canvas x y tile)
  (when (>= (length history) HISTORY-MAX) (drop history HISTORY-DROP))
  (define new-history (history-add-action history (action 'atomic (list (list (send scene get x y) x y)))))
  (send scene set x y tile)
  (send canvas draw-tile tile x y #t)
  new-history)

; Test if colors are equal
(define (color-equal? c1 c2) (and (= (send c1 red) (send c2 red))
                                  (= (send c1 green) (send c2 green))
                                  (= (send c1 blue) (send c2 blue))
                                  (= (send c1 alpha) (send c2 alpha))))
(define (tile-equal? a b)
  (and (equal? (tile-symbol a) (tile-symbol b))
       (color-equal? (tile-fg a) (tile-fg b))
       (color-equal? (tile-bg a) (tile-bg b))
       (equal? (tile-descr a) (tile-descr b))))