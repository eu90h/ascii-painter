#lang racket

(provide set-and-add-to-history undo-last-action history-add-action (struct-out action) history-add-actions)

(require "scene.rkt")

(struct action (type change-data))


(define HISTORY-MAX 2000) ; max number of items in history
(define HISTORY-DROP 500) ; how many actions to remove from the history after going over HISTORY-MAX

(define (history-add-action history a) (when (action? a) (append history (list a))))

(define (history-add-actions history actions) 
	(if (null? actions) history (history-add-actions 
		(history-add-action history (first actions))
		(rest actions))))

(define (set-and-add-to-history history scene x y tile)
	(when (>= (length history) HISTORY-MAX) (drop history HISTORY-DROP))
	(define new-history (history-add-action history (action 'paint (list (list (send scene get x y) x y)))))
  	(send scene set x y tile)
  	new-history)

(define (undo-paint-action scene action-datum)
	(let* ([t (first action-datum)] [x (second action-datum)] [y (third action-datum)])
		(send scene set x y t)))

(define (undo-line-action scene action-data)
	(map ((curry undo-paint-action) scene) action-data))

(define (undo-last-action history scene)
	(if (null? history) history
		(let ([last-action (last history)])
			(case (action-type last-action)
				[(paint) (undo-paint-action scene (first (action-change-data last-action)))]
				[(line) (undo-line-action scene (action-change-data last-action))])
			(if (<= (length history) 1) (set! history null)
      		(take history (sub1 (length history)))))))