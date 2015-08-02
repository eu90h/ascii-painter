#lang racket

(provide undo-last-action history-add-action (struct-out action) history-add-actions)

(require "scene.rkt")

; type is either 'atomic or 'compound
; chane-data is a list of tiles
(struct action (type change-data))

; List Action -> List
; Appends an action to a list (pretty much a wrapper over append)
(define (history-add-action history a) (when (action? a) (append history (list a))))

; History List[Action] -> History
; Adds a list of actions to a history
(define (history-add-actions history actions) 
	(if (null? actions) history (history-add-actions 
		(history-add-action history (first actions))
		(rest actions))))

; Scene Action -> Void
; Undos the placement of a single tile
(define (undo-atomic-action scene action)
	(let* ([action-datum (first (action-change-data action))] [t (first action-datum)] 
			[x (second action-datum)] [y (third action-datum)])
		(send scene set x y t)))

; Scene Action -> Void
; Undos a line
(define (undo-compound-action scene compound-action)
	(map ((curry undo-atomic-action) scene) (action-change-data compound-action)))

; History Scene -> History
; undos the last action in the given history on the given scene
(define (undo-last-action history scene)
	(if (null? history) history
		(let ([last-action (last history)])
			(case (action-type last-action)
				[(atomic) (undo-atomic-action scene last-action)]
				[(compound) (undo-compound-action scene last-action)])
			(if (<= (length history) 1) null
      		(take history (sub1 (length history)))))))