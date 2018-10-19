#lang racket

(require racket/draw)
(require racket/gui)

(require "draw-utils.rkt")
          
; drawing stuff
(define target (make-bitmap 2500 2500))
(define dc (new bitmap-dc% [bitmap target]))

(send dc scale 200 200)
(send dc translate 0 0)
(send dc set-brush "white" 'transparent)
(send dc set-pen "black" 0.01 'solid)
(send dc set-smoothing 'aligned)

(send dc draw-path
  (bezier-from-anchors dc
    (anchor-point (vector 3 2) (vector 0 0) (vector 0 1))
    (anchor-point (vector 3 6) (vector 5 0) (vector 0 0))))

(send target save-file "pic.png" 'png)
(make-object image-snip% target)
