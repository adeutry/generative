#lang racket

(require racket/draw)
(require racket/gui)

(define (sum l)
  (foldr + 0 l))

(define (zip a b)
  (if (or (null? a) (null? b)) '()
      (cons (list (first a) (first b)) (zip (rest a) (rest b)))))

(define (eval-poly x l)
  (let
      ([powers (map (lambda (n) (expt x n)) (range (length l)))])
      (sum (map * powers l))))

(define (vec-mult c v)
  (vector-map (λ (x) (* x c)) v))

(define (vec-add va vb)
  (vector-map (λ (a b) (+ a b)) va vb))

(define (bezier-two pa pb)
  (λ (t)
    (vec-add
       (vec-mult t pb)
       (vec-mult (- 1 t) pa))))

(define (bezier-three pa pb pc)
  (λ (t)
    (vec-add
       (vec-mult t
                 ((bezier-two pb pc) t))
       (vec-mult (- 1 t)
                 ((bezier-two pa pb) t)))))

; sample beziers
(define bezier-a (bezier-two (vector 1 1) (vector 2 2)))
(define bezier-b
  (bezier-three
     (vector 1 1)
     (vector 1 3)
     (vector 2 2)))

(define bezier-b-points
  (map bezier-b (range 0 1 0.01)))

(define (draw-curve-points ps)
  (let
      ([p (new dc-path%)]
       [h (first ps)]
       [r (rest ps)])
    (send p move-to (vector-ref h 0) (vector-ref h 1))
    (for-each
      (λ (v) (send p line-to (vector-ref v 0) (vector-ref v 1))) r)
    p))


; drawing stuff
(define target (make-bitmap 300 300))
(define dc (new bitmap-dc% [bitmap target]))

; scale and translate canvas
(send dc scale 80 80)
(send dc translate 0 0)
(send dc set-pen "black" 0.01 'solid)

; draw paths
(send dc draw-path (draw-curve-points bezier-b-points))

; display in drracket gui
(make-object image-snip% target)
  