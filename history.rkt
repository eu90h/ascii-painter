#lang racket

(provide set-and-add-to-history undo-last-action add-action-to-history (struct-out action))

(require "scene.rkt")

(struct action (type change-data))

(define history null)

(define HISTORY-MAX 2000) ; max number of items in history
(define HISTORY-DROP 500) ; how many actions to remove from the history after going over HISTORY-MAX

(define (add-action-to-history a)
	(set! history (append history (list a))))

(define (set-and-add-to-history scene x y tile)
	(when (>= (length history) HISTORY-MAX) (drop history HISTORY-DROP))
	(add-action-to-history (action 'paint (list (list (send scene get x y) x y))))
  	(send scene set x y tile))

(define (undo-paint-action scene action-datum)
	(let* ([t (first action-datum)] [x (second action-datum)] [y (third action-datum)])
		(send scene set x y t)
    	(if (<= (length history) 1) (set! history null)
      		(set! history (take history (sub1 (length history)))))))

(define (undo-line-action scene action-data)
	(map ((curry undo-paint-action) scene) action-data))

(define (undo-last-action scene)
	(unless (zero? (length history))
		(let ([last-action (last history)])
			(case (action-type last-action)
				[(paint) (undo-paint-action scene (first (action-change-data last-action)))]
				[(line) (undo-line-action scene (action-change-data last-action))]))))