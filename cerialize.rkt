; cerialize - serializes data to csv format (particularly structs)

#lang racket/gui

(require racket/unsafe/ops "scene.rkt")

(provide cerialize-struct cerialize-color% cerialize-string cerialize-number cerialize-char uncerialize-line)

(define (cerialize-struct s num-fields out)
	(define (loop i)
		(unless (>= i num-fields) 
			(let ([v (unsafe-struct-ref s i)]) ; yeah this is crazy!
				(cond [(char? v) (cerialize-char v out)]
						[(and (object? v) (is-a? v color%)) (cerialize-color% v out)]
						[(string? v) (cerialize-string v out)]
						[(number? v) (cerialize-number v out)]))
			(unless (= (add1 i) num-fields) (display "," out))
			(loop (+ 1 i))))
	(loop 0)
	(newline out))

(define (cerialize-string s out)
	(when (and (string? s) (output-port? out))
		(display s out)))

(define (cerialize-number n out)
	(when (and (number? n) (output-port? out))
		(display (number->string n) out)))

(define (cerialize-char c out)
	(when (and (char? c) (output-port? out))
		(display (string c) out)))

(define (cerialize-color% c out)
	(when (and (object? c) (is-a? c color%) (output-port? out))
		(cerialize-number (send c red) out)
		(display "," out)
		(cerialize-number (send c green) out)
		(display "," out)
		(cerialize-number (send c blue) out)
		(display "," out)
		(cerialize-number (send c alpha) out)))

(define (uncerialize-line line)
	(define parts (string-split line ","))
	(define symbol (string-ref (first parts) 0))
	(define fg-r (string->number (second parts)))
	(define fg-g (string->number (third parts)))
	(define fg-b (string->number (fourth parts)))
	(define fg-a (string->number (fifth parts)))
	(define bg-r (string->number (sixth parts)))
	(define bg-g (string->number (seventh parts)))
	(define bg-b (string->number (eighth parts)))
	(define bg-a (string->number (ninth parts)))
	(define descr (tenth parts))
	(define fg (make-object color% fg-r fg-g fg-b fg-a))
	(define bg (make-object color% bg-r bg-g bg-b bg-a))
	(tile symbol fg bg descr))


