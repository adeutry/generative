#lang racket

(require racket/draw)
(require racket/gui)

(require "draw-utils.rkt")
          
; drawing stuff
(define target (make-bitmap 1000 1000))
(define dc (new bitmap-dc% [bitmap target]))

(send dc scale 75 75)
(send dc translate 0 0)
(send dc set-brush "white" 'transparent)
(send dc set-pen "black" 0.01 'solid)
(send dc set-smoothing 'aligned)

(define (leaf pos-vec angle)
  (let* ([rotate-and-move (lambda (a) (vec-add pos-vec (vec-rotate a angle)))]
         [bot pos-vec]
         [top      (rotate-and-move (vector 7 0))]
         [h1-left  (rotate-and-move (vector 2 -4))]
         [h1-right (rotate-and-move (vector 2 4))]
         [h2-left  (rotate-and-move (vector 4 -1))]
         [h2-right (rotate-and-move (vector 4 1))]
         [h1-span-func (bezier (list h1-left h1-right))]
         [h2-span-func (bezier (list h2-left h2-right))])
  (map (λ (x) (draw-bezier (list
                    bot
                    (h1-span-func x)
                    (h2-span-func x)
                    top)))
       (reverse (range 0 1 0.1)))))

#;(define b (draw-bezier (list
  (vector 1 1)
  (vector 2 4)
  (vector 4 2)
  (vector 7 1))))

(for-each (λ (p) 
   (send dc draw-path p)) 
       (leaf (vector 4 4) 0))

(send target save-file "pic.png" 'png)
(make-object image-snip% target)
