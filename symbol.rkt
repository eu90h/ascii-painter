#lang racket

(provide (contract-out
	[cp437-strings list?]
	[string->symbol (-> string? char?)]
	[symbol->string (-> char? string?)]
	[index->symbol (-> natural-number/c char?)]
	[symbol->index (-> char? natural-number/c)]))

(define cp437-strings (list
	"Null"  "☺" "☻" "♥" "♦" "♣" "♠" "•" "◘" "○" "◙" "♂" "♀" "♪" "♫" "☼"
	"►" "◄" "↕" "‼" "¶" "§" "▬" "↨" "↑" "↓" "→" "←" "∟" "↔" "▲" "▼"
	"Space"
	"!" "\"" "#" "$" "%" "&" "'" "(" ")" "*" "+" "," "-" "." "/"
	"0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
	":" ";" "<" "="	">" "?" "@"
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
	"≡" "±" "≥" "≤" "⌠" "⌡" "÷" "≈" "°" "∙" "·" "√" "ⁿ" "²" "■"))

; String -> Natural
; Given a string representation of a cp437 glyph, returns the index of that glyph in cp437-strings
(define (symbol->index sym)
  (define (f i)
    (when (< i (length cp437-strings))
      (if (eq? (list-ref cp437-strings i) sym) i (f (add1 i)))))
  (f 0))

; String -> Char
; returns the character representation of a given cp437 glyph string
(define (string->symbol string)
	(define (f i l) (cond [(null? l) 0]
		[(eq? (first l) string) i]
		[else (f (add1 i) (rest l))]))
	(integer->char (f 0 cp437-strings)))

; Natural -> Char
; Given an index into cp437-strings, returns the associated character
(define index->symbol (compose string->symbol ((curry list-ref) cp437-strings)))

; Char -> String
; Given a character, returns the associated element of cp437-strings
(define symbol->string (compose ((curry list-ref) cp437-strings) char->integer))