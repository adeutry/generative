#lang racket

(require racket/draw)
(require racket/gui)

(require "draw-utils.rkt")
          
; drawing stuff
(define target (make-bitmap 1500 1500))
(define dc (new bitmap-dc% [bitmap target]))

(send dc scale 140 140)
(send dc translate 0 0)
(send dc set-brush "white" 'transparent)
(send dc set-pen "black" 0.01 'solid)
(send dc set-smoothing 'aligned)

(define (leaf pos-vec angle)
  (let* ([rotate-and-move (lambda (a) (vec-add pos-vec (vec-rotate a angle)))]
         [bot pos-vec]
         [bot-pos 1]
         [top-pos 3]
         [bot-width 4]
         [top-width 1]
         [top      (rotate-and-move (vector 7 0))]
         [h1-left  (rotate-and-move (vector bot-pos (- bot-width)))]
         [h1-right (rotate-and-move (vector bot-pos bot-width))]
         [h2-left  (rotate-and-move (vector top-pos (- top-width)))]
         [h2-right (rotate-and-move (vector top-pos top-width))]
         [h1-span-func (bezier (list h1-left h1-right))]
         [h2-span-func (bezier (list h2-left h2-right))])
  (map (λ (x) (draw-bezier (list
                    bot
                    (h1-span-func x)
                    (h2-span-func x)
                    top)))
       (reverse (range 0 1 0.1)))))

(define a (bezier (list
  (vector 5 10))))

(define b (bezier (list
  (vector 1 1)
  (vector 1 5)
  (vector 1 6)
  (vector 1 8))))

(define c (bezier (list
  (vector 10 8)
  (vector 9 8)
  (vector 8 8)
  (vector 7 8))))

(define d (bezier (list
  (vector 8 10)
  )))

#;(for-each (λ (p) 
   (send dc draw-path p)) 
       (leaf (vector 4 4) 0))

(for-each (λ (p)
    (send dc draw-path 
      (draw-bezier (list
        (a p)
        (b p)
        (c p)
        (d p)))))
  (range 0 1 0.05))

(send target save-file "pic.png" 'png)
(make-object image-snip% target)
