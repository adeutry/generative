#lang racket

(require racket/draw)
(require racket/gui)

(require "draw-utils.rkt")
          
; drawing stuff
(define target (make-bitmap 1000 1000))
(define dc (new bitmap-dc% [bitmap target]))

(send dc scale 100 100)
(send dc translate 0 0)
(send dc set-brush "white" 'transparent)
(send dc set-pen "black" 0.01 'solid)
(send dc set-smoothing 'aligned)

(define (curves pos-vec angle)
  (let* ([bot pos-vec]
         [top  (vec-add pos-vec (vec-rotate (vector 2 0) angle))]
         [left (vec-add pos-vec (vec-rotate (vector 1 -1) angle))]
         [right (vec-add pos-vec (vec-rotate (vector 1 1) angle))]
         [span-func (bezier (list left right))]) ; use two-point bezier curve for horizontal span
  (map (λ (x) (draw-bezier-three
                    bot
                    (span-func x)
                    top))
       (reverse (range 0 1 0.1)))))

(map
  (λ (d) (for-each (λ (p) (send dc draw-path p)) (curves (vector 5 5) d)))
  (range 0 360 30))

(send target save-file "pic.png" 'png)
(make-object image-snip% target)
