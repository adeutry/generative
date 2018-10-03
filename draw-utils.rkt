#lang racket

(require racket/draw)
(require racket/gui)

(provide (all-defined-out))

; vectors
(define (vec-mult c v)
  (vector-map (λ (x) (* x c)) v))

(define (vec-add va vb)
  (vector-map (λ (a b) (+ a b)) va vb))

(define (vec-rotate v deg)
  (let ([x (vector-ref v 0)]
        [y (vector-ref v 1)]
        [r (degrees->radians deg)])
    (vector
       (- (* x (cos r)) (* y (sin r)))
       (+ (* x (sin r)) (* y (cos r))))))

; [vector] -> Float -> [vector]
(define (bezier points)
    (lambda (t) 
      (match points
        [(list a) a]
        [ _ (let
          ([first-curve  (bezier (drop-right points 1))]
          [second-curve (bezier (rest points))])
          (vec-add 
            (vec-mult t (second-curve t))
            (vec-mult (- 1 t) (first-curve t))))])))

; [vector] -> Unit
(define (draw-curve-points ps)
  (let
      ([p (new dc-path%)]
       [h (first ps)]
       [r (rest ps)])
    (send p move-to (vector-ref h 0) (vector-ref h 1))
    (for-each
      (λ (v) (send p line-to (vector-ref v 0) (vector-ref v 1))) r)
    p))

(define (draw-bezier-three pa pb pc)
   (draw-curve-points
      (map
         (bezier (list pa pb pc))
         (range 0 1 0.01))))

(define (draw-bezier points)
   (draw-curve-points
      (map
         (bezier points)
         (range 0 1 0.01))))

(define foo (bezier (list
  (vector 2 2)
  (vector 1 4)
  (vector 3 6)
  (vector 4 4))))
