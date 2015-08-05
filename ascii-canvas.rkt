#lang racket

(require 
 racket/draw
 racket/gui
 racket/path
 racket/runtime-path
 racket/unsafe/ops
 "scene.rkt")

(provide
 (all-defined-out))

(define-runtime-path RUNTIME_DIR "./")
(define-syntax-rule (scope body* ...) (let () body* ...))
  
; A simple matrix ADT
(define-struct matrix (width height data) #:mutable #:constructor-name make-matrix-struct)
(define (unsafe-matrix-width m) (unsafe-struct-ref m 0))
(define (unsafe-matrix-height m) (unsafe-struct-ref m 1))
(define (unsafe-matrix-data m) (unsafe-struct-ref m 2))

(define (make-matrix width height [gen (lambda (x y) #f)]) 
  (make-matrix-struct width height
    (for/vector ([i (in-range (* width height))])
      (gen (quotient i height) (remainder i height)))))

(define (matrix-ref matrix x y) 
  (unsafe-vector-ref (unsafe-matrix-data matrix) (unsafe-fx+ y (unsafe-fx* x (unsafe-matrix-height matrix)))))

(define (matrix-set! matrix x y val) 
  (unsafe-vector-set! (unsafe-matrix-data matrix) (unsafe-fx+ y (unsafe-fx* x (unsafe-matrix-height matrix))) val))

; Convert a color name, list of rgb/rgba, vector of rgb/rgba or multiple arguments to a color
(define ->color
  (case-lambda 
    [(arg)
     (cond
       [(is-a? arg color%) arg]
       [(string? arg) (make-object color% arg)]
       [(list? arg) (apply make-object (cons color% arg))]
       [(vector? arg) (apply make-object (cons color% (vector->list arg)))])]
    [(r g b)
     (make-object color% r g b)]
    [(r g b a)
     (make-object color% r g b a)]))

; Test if colors are equal
(define (color-equal? c1 c2) (and (= (send c1 red) (send c2 red))
                                  (= (send c1 green) (send c2 green))
                                  (= (send c1 blue) (send c2 blue))
                                  (= (send c1 alpha) (send c2 alpha))))

; A canvas to draw on with ASCII characters
(define ascii-canvas%
  (class canvas%
    (inherit
      get-width
      get-height
      refresh)
    
    (init-field
     parent
     [tileset-filename "cp437_16x16.png"]
     [width-in-characters 80]
     [height-in-characters 24])
    
    (field
     [offscreen-buffer #f] 
     [offscreen-buffer-dc #f]
     [char-width 9]
     [char-height 16]
     [default-background-color (make-object color% 0 0 0)]
     [default-foreground-color (make-object color% 255 255 255)]
     [cursor-x 0]
     [cursor-y 0]
     [glyphs #f]
     [chars #f]
     [background-colors #f]
     [foreground-colors #f]
     [old-chars #f]
     [old-background-colors #f]
     [old-foreground-colors #f])
    
    ; Height in pixels of a character
    (define/public (get-char-height) char-height)
    
    ; Width in pixels of a character
    (define/public (get-char-width) char-width)
    
    ; Height in characters of the canvas
    (define/public (get-height-in-characters) height-in-characters)
    
    ; Width in characters of the canvas
    (define/public (get-width-in-characters) width-in-characters)
    
    ; The x coordinate of the cursor in tiles
    (define/public (get-cursor-x) cursor-x)
    (define/public (set-cursor-x x)
      (when (or (not (integer? x)) (< x 0) (>= x width-in-characters))
        (raise-argument-error 'set-cursor-x (format "integer in range [0, ~a]" (- width-in-characters 1)) x))
      (set! cursor-x x))
    
    ; The y coordinate of the cursor in tiles
    (define/public (get-cursor-y) cursor-y)
    (define/public (set-cursor-y y)
      (when (or (not (integer? y)) (< y 0) (>= y height-in-characters))
        (raise-argument-error 'set-cursor-y (format "integer in range [0, ~a]" (- height-in-characters 1)) y))
      (set! cursor-y y))
    
    ; Set both the x and y of the cursor position
    (define/public (set-cursor-position x y)
      (set-cursor-x x)
      (set-cursor-y y))
    
    ; Control the default background color used to print tiles if no other color is given
    (define/public (get-default-background-color) default-background-color)
    (define/public (set-default-background-color . args)
      (define argc (length args))
      (when (or (not (member argc '(1 3 4)))
                (and (= argc 1) (not (string? (car args))))
                (and (or (= argc 3) (= argc 4)) (not (andmap byte? args))))
        (raise-argument-error 'set-default-background-color "color name or rgb (as bytes) or rgba (as bytes)" args))
      (set! default-background-color (apply make-object (cons color% args))))
        
    ; Control the default foreground color used to print tiles if no other color is given
    (define/public (get-default-foreground-color) default-foreground-color)
    (define/public (set-default-foreground-color . args)
      (define argc (length args))
      (when (or (not (member argc '(1 3 4)))
                (and (= argc 1) (not (string? (car args))))
                (and (or (= argc 3) (= argc 4)) (not (andmap byte? args))))
        (raise-argument-error 'set-default-foreground-color "color name or rgb (as bytes) or rgba (as bytes)" args))
      (set! default-foreground-color (apply make-object (cons color% args))))
    
    ; Repaint the canvas
    (define/private (my-paint-callback self dc)
      ; If the buffer hasn't been defined, do that
      (unless offscreen-buffer
        (set! offscreen-buffer (make-screen-bitmap (get-width) (get-height)))
        (set! offscreen-buffer-dc (new bitmap-dc% [bitmap offscreen-buffer])))
      
      ; Draw any new characters to the buffer
      (for* ([x (in-range width-in-characters)]
             [y (in-range height-in-characters)]
             #:when (or (not (eq? (matrix-ref chars x y) (matrix-ref old-chars x y)))
                        (not (color-equal? (matrix-ref background-colors x y) (matrix-ref old-background-colors x y)))
                        (not (color-equal? (matrix-ref foreground-colors x y) (matrix-ref old-foreground-colors x y)))))
        ; Draw the glyph to the buffer
        (define src-c (char->integer (matrix-ref chars x y)))
        (define src-x (unsafe-fxremainder src-c 16))
        (define src-y (unsafe-fxquotient src-c 16))
        
        ; Draw the foreground 
        ; NOTE: Not a mistake. Yes, it is weird.
        (send offscreen-buffer-dc set-brush (new brush% [color (matrix-ref foreground-colors x y)]))
        (send offscreen-buffer-dc draw-rectangle
              (unsafe-fx* x char-width)
              (unsafe-fx* y char-height)
              char-width
              char-height)
        
        ; Overlay the background
        ; NOTE: Not a mistake. Yes, it is weird.
        (send offscreen-buffer-dc draw-bitmap-section
              glyphs
              (unsafe-fx* x char-width)
              (unsafe-fx* y char-height)
              (unsafe-fx* src-x char-width)
              (unsafe-fx* src-y char-height)
              char-width
              char-height
              'solid ; could be solid, opaque, or xor
              (matrix-ref background-colors x y)
              glyphs)
        
        ; Update maps
        (matrix-set! old-chars x y (matrix-ref chars x y))
        (matrix-set! old-background-colors x y (matrix-ref background-colors x y))
        (matrix-set! old-foreground-colors x y (matrix-ref foreground-colors x y)))
      
      ; Finally, flip the buffers
      (send dc draw-bitmap offscreen-buffer 0 0))
    
    ; Clear the screen
    (define/public (clear)
      (for* ([xi (in-range 0 width-in-characters)]
             [yi (in-range 0 height-in-characters)])
        (send this write #\space xi yi default-foreground-color default-background-color)))
    
    ; Write a single character
    (define/public (write char x y fg bg)
      (matrix-set! chars x y char)
      (matrix-set! foreground-colors x y fg)
      (matrix-set! background-colors x y bg)
                   
      (set! cursor-x (unsafe-fx+ x 1))
      (set! cursor-y (unsafe-fx+ y 1)))
    
    (define/public (write-tile x y t)
      (matrix-set! chars x y (unsafe-tile-symbol t))
      (matrix-set! foreground-colors x y (unsafe-tile-fg t))
      (matrix-set! background-colors x y (unsafe-tile-bg t))
                   
      (set! cursor-x (unsafe-fx+ x 1))
      (set! cursor-y (unsafe-fx+ y 1)))
        
    ; Write a string
    (define/public write-string
      (case-lambda
        [(str)
         (send this write-string str cursor-x cursor-y default-foreground-color default-background-color)]
        [(str foreground)
         (send this write-string str cursor-x cursor-y foreground default-background-color)]
        [(str x/foreground y/background)
         (if (and (number? x/foreground) (number? y/background))
             (send this write-string str x/foreground y/background default-foreground-color default-background-color)
             (send this write-string str cursor-x cursor-y x/foreground y/background))]
        [(str x y foreground)
         (send this write-string str x y foreground default-background-color)]
        [(str x y foreground background)
         (for ([c (in-string str)]
               [i (in-naturals)])
           (send this write c (unsafe-fx+ x i) y foreground background))]))
    
    ; Write a string centered horizontally
    (define/public write-center
      (case-lambda
        [(str y)
         (send this write-center str y default-foreground-color default-background-color)]
        [(str y foreground)
         (send this write-center str y foreground default-background-color)]
        [(str y foreground background)
         (define x (- (quotient width-in-characters 2) (quotient (string-length str) 2)))
         (send this write-string str x y foreground background)]))
    
    ; Map a function across the tiles
    ; Functions should be of the form: x y char fg bg => char fg bg
    (define/public for-each-tile
      (case-lambda
       [(f)
        (send this for-each-tile 0 0 width-in-characters height-in-characters f)]
       [(x y width height f)
        (unless (procedure? f)
          (raise-argument-error 'for-each-tile "function?" f))
        (unless (<= 0 x (- width-in-characters 1))
          (raise-argument-error 'for-each-tile (format "x in the range [0, ~a)" width-in-characters) x))
        (unless (<= 0 y (- height-in-characters 1))
          (raise-argument-error 'for-each-tile (format "y in the range [0, ~a)" height-in-characters) y))
        (unless (> 0 width)
          (raise-argument-error 'for-each-tile (format "positive integer? for width" width-in-characters width)))
        (unless (> 0 height)
          (raise-argument-error 'for-each-tile (format "positive integer? for height" height-in-characters height)))
        (unless (< (+ x width) width-in-characters)
          (raise-argument-error 'for-each-tile (format "x+width in range [0, ~a)" width-in-characters (+ x width))))
        (unless (< (+ y height) height-in-characters)
          (raise-argument-error 'for-each-tile (format "y+height in range [0, ~a)" height-in-characters (+ y height))))
        
        (for* ([xi (in-range x (unsafe-fx+ x width))]
               [yi (in-range y (unsafe-fx+ y height))])
          (define-values (char fg bg)
            (f xi yi (matrix-ref chars xi yi) (matrix-ref foreground-colors xi yi) (matrix-ref background-colors xi yi)))
          (matrix-set! chars xi yi char)
          (matrix-set! foreground-colors xi yi fg)
          (matrix-set! background-colors xi yi bg))]))
        
    
    ; Validate that the width and height make sense
    (when (unsafe-fx<= width-in-characters 0) (raise-argument-error 'ascii-canvas% "positive integer" width-in-characters))
    (when (unsafe-fx<= height-in-characters 0) (raise-argument-error 'ascii-canvas% "positive integer" height-in-characters))
    
    ; Set up the color / char arrays
    (set! chars (make-matrix width-in-characters height-in-characters (lambda (x y) (integer->char 0))))
    (set! foreground-colors (make-matrix width-in-characters height-in-characters (lambda (x y) (make-object color% "white"))))
    (set! background-colors (make-matrix width-in-characters height-in-characters (lambda (x y) (make-object color% "black"))))
    
    (set! old-chars (make-matrix width-in-characters height-in-characters (lambda (x y) (integer->char 0))))
    (set! old-foreground-colors (make-matrix width-in-characters height-in-characters (lambda (x y) (make-object color% "white"))))
    (set! old-background-colors (make-matrix width-in-characters height-in-characters (lambda (x y) (make-object color% "black"))))
    
    ; Load the glyphs
    (define glyph-file (read-bitmap (build-path RUNTIME_DIR tileset-filename) 'unknown/alpha (->color "magenta")))
    (set! glyphs (make-monochrome-bitmap (send glyph-file get-width) (send glyph-file get-height)))
    (define glyphs-dc (new bitmap-dc% [bitmap glyphs]))
    (send glyphs-dc draw-bitmap glyph-file 0 0)
    
    (set! char-width (unsafe-fxquotient (send glyphs get-width) 16))
    (set! char-height (unsafe-fxquotient (send glyphs get-height) 16))
    
 
    ; Create the canvas
    (super-new 
     [parent parent]
     [paint-callback (lambda (c dc) (my-paint-callback c dc))]
     [min-width (unsafe-fx* width-in-characters char-width)]
     [min-height (unsafe-fx* height-in-characters char-height)])
    
    ; Do an initial clear
    (send this clear)
    (send this focus)))