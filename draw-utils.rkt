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


; beziers
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

(define (bezier points)
    (lambda (t) 
      (match points
        [(list a) a]
        [ _ (let
          ([first-curve  (bezier (rest (reverse points)))]
          [second-curve (bezier (rest points))])
          (vec-add 
            (vec-mult t (first-curve t))
            (vec-mult (- 1 t) (second-curve t))))])))

(define bez (bezier '((vector 1 2) (vector 3 4))))

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
         (bezier-three pa pb pc)
         (range 0 1 0.01))))
