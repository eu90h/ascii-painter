#lang racket/gui
(require framework/test "main.rkt")
(sleep 10)
(displayln "hey")
(test:number-pending-actions)
(test:menu-select "File" "New")
(test:number-pending-actions)