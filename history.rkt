#lang racket

(provide set-and-add-to-history undo-last-action)

(require "scene.rkt")

(define history null)

(define HISTORY-MAX 2000) ; max number of items in history
(define HISTORY-DROP 500) ; how many actions to remove from the history after going over HISTORY-MAX

(define (set-and-add-to-history scene x y tile)
	(when (>= (length history) HISTORY-MAX) (drop history HISTORY-DROP))
  	(set! history (append history (list (list (send scene get x y) x y))))
  	(send scene set x y tile))

(define (undo-last-action scene)
	(unless (zero? (length history))
  		(let* ([last-action (last history)] [t (first last-action)] [x (second last-action)] [y (third last-action)])
    		(send scene set x y t)
    		(if (= 1 (length history)) (set! history null)
      			(set! history (take history (sub1 (length history))))))))