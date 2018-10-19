#lang racket

(require racket/draw)
(require racket/gui)

(require "draw-utils.rkt")
          
; drawing stuff
(define target (make-bitmap 2500 2500))
(define dc (new bitmap-dc% [bitmap target]))

(send dc scale 150 150)
(send dc translate 0 0)
(send dc set-brush "white" 'transparent)
(send dc set-pen "black" 0.01 'solid)
(send dc set-smoothing 'aligned)

(define points
  (list
    (vector 5 4) 
    (vector 11 4)
    (vector 11 12)
    (vector 5 8)))

(map
  (lambda (x) (send dc draw-path x))
  (points-to-loop-bezier dc points))

(send target save-file "pic.png" 'png)
(make-object image-snip% target)
