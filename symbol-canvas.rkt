#lang racket/gui

(provide symbol-canvas%)

(require ascii-canvas "scene.rkt" "point.rkt" "util.rkt" "history.rkt" "brush.rkt")

(define symbol-canvas% (class ascii-canvas%
  ; Frame% Scene% (Tile Pair -> Any)
  (init-field container scene set-tile-callback)
  ; container is a parent in which the canvas is placed.
  ; set-tile-callback is a function taking a tile and a pair of two integers representing 
  ; the table offset (see cur-tile-table-offset)

  ; this holds the tile the user has chosen
  (field [cur-tile empty-tile])

  ; this holds the indices into the data field corresponding to the current tile
  (field [cur-tile-table-offset '(0 0)]) 

  (field [width 16] [height 16]) ; dimensions of the canvas

  (super-new [parent container] [width-in-characters width] [height-in-characters height])

  ; the x-scale and y-scale are crucial for clamping mouse clicks to the scene grid
  (field [x-scale (/ (send this get-width) width)] [y-scale (/ (send this get-height) height)])

  ; these intervals represent the boundaries of the symbol-canvas
  ;(field [x-interval (interval 0 (sub1 width))] [y-interval (interval 0 (sub1 height))])

  (field [cp437-strings (list
  "Null"  "☺" "☻" "♥" "♦" "♣" "♠" "•" "◘" "○" "◙" "♂" "♀" "♪" "♫" "☼"
  "►" "◄" "↕" "‼" "¶" "§" "▬" "↨" "↑" "↓" "→" "←" "∟" "↔" "▲" "▼"
  "Space"
  "!" "\"" "#" "$" "%" "&" "'" "(" ")" "*" "+" "," "-" "." "/"
  "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
  ":" ";" "<" "=" ">" "?" "@"
  "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
  "[" "\\" "]" "^" "_" "`"
  "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
  "{" "|" "}" "~""⌂" 
  "Ç" "ü" "é" "â" "ä" "à" "å" "ç" "ê" "ë" "è" "ï" "î" "ì" "Ä" "Å"
  "É" "æ" "Æ" "ô" "ö" "ò" "û" "ù" "ÿ" "Ö" "Ü" "¢" "£" "¥" "₧" "ƒ"
  "á" "í" "ó" "ú" "ñ" "Ñ" "ª" "º" "¿" "⌐" "¬" "½" "¼" "¡" "«" "»"
  "░" "▒" "▓" "│" "┤" "╡" "╢" "╖" "╕" "╣" "║" "╗" "╝" "╜" "╛" "┐"
  "└" "┴" "┬" "├" "─" "┼" "╞" "╟" "╚" "╔" "╩" "╦" "╠" "═" "╬" "╧"
  "╨" "╤" "╥" "╙" "╘" "╒" "╓" "╫" "╪" "┘" "┌" "█" "▄" "▌" "▐" "▀"
  "α" "ß" "Γ" "π" "Σ" "σ" "µ" "τ" "Φ" "Θ" "Ω" "δ" "∞" "φ" "ε" "∩"
  "≡" "±" "≥" "≤" "⌠" "⌡" "÷" "≈" "°" "∙" "·" "√" "ⁿ" "²" "■")])

  ; this a 2d vector containing all the cp437 glyphs represented as tile structs
  (field [data (for/vector ([x width]) 
            (for/vector ([y height]) 
              (tile (integer->char (modulo (+ (* height y) x) 255)) 
                    (make-object color% 255 255 255 1.0) 
                    (make-object color% 0 0 0 1.0)
                    (list-ref cp437-strings (modulo (+ (* height y) x) 255)))))])

  ; Integer Integer -> Void
  ; sets the scale of the x-axis and y-axis
  (define/public (set-scales x y) (set! x-scale x) (set! y-scale y))

  ; Void -> Tile
  (define/public (get-tile) cur-tile)

  ; Void -> Pair
  (define/public (get-table-offset) cur-tile-table-offset)

  ; Real Real -> Pt
  ; takes two real numbers (x, y) and returns an integer point representing offsets into the data field
  (define/public (clamp mx my)
    (pt (floor (/ mx x-scale))
        (floor (/ my y-scale))))

  ; (Tile -> Tile) -> Void
  ; given a callback that takes and returns tiles, applies it to all tiles in the data field and updates them with the
  ; output of the callback
  (define (process-tiles fn) (for* ([x (in-range width)] [y (in-range height)])
    (set x y (fn (send this symbol-table-lookup x y)))))

  ; Integer Integer Tile -> Void
  (define (set x y tile) (when (good-xy? x y) (vector-set! (vector-ref data x) y tile)))

  ; Color% Color% -> Void
  ; changes the colors of all the tiles in the data field
  (define/public (dye-tiles fg bg)
    (process-tiles (lambda (t) (tile (tile-symbol t) fg bg (tile-descr t))))
    (draw-symbol-table)
    (send container refresh))

  ; Evt -> Void
  ; called every time a mouse event happens
  (define/override (on-event mouse-event)
    (when (eq? 'left-up (send mouse-event get-event-type))
    (let* ([p (send this clamp (send mouse-event get-x) (send mouse-event get-y))])
      (set! cur-tile-table-offset (list (pt-x p) (pt-y p)))
      (set! cur-tile (symbol-table-lookup (pt-x p) (pt-y p)))
      (set-tile-callback cur-tile cur-tile-table-offset))))

  ; Void -> Void
  (define/public (draw)
    (draw-symbol-table) 
    (send container refresh))

  ; Integer Integer -> Boolean
  ; ensures the integers lie within the canvas boundaries
  (define (good-xy? x y)
    (and (>= y 0) (>= x 0) (< x width) (< y height)))

  ; Integer Integer -> Tile
  (define/public (symbol-table-lookup x y) 
    (if (good-xy? x y) 
      (vector-ref (vector-ref data x) y) 
      (vector-ref (vector-ref data 0) 0)))

  ; Tile Integer Integer -> Void
  ; draws a single tile to the canvas
  (define/public (draw-tile tile x y)
    (send this write (tile-symbol tile) x y (tile-fg tile) (tile-bg tile)))

  ; Void -> Void
  ; draws the entire symbol table to the canvas
  (define (draw-symbol-table)
    (for* ([x (in-range width)] [y (in-range height)])
      (draw-tile (symbol-table-lookup x y) x y)))))

(define (color? c) (and (object? c) (is-a? c color%)))
(define (canvas? c) (and (object? c) (is-a? c canvas%)))
(define (event? e) (and (object? e) (is-a? e event%)))
(define (scene? e) (and (object? e) (is-a? e scene%)))
(define tile/c (struct/c tile char? color? color? string?))
(define/contract symbol-canvas+c%
  (class/c [on-event (->m event? void?)]
    [draw (->m void?)]
    [draw-tile (->m tile/c natural-number/c natural-number/c void?)])
  symbol-canvas%)