#lang racket

(require racket/draw)
(require racket/gui)

(provide (all-defined-out))

; debug stuff
(define debug-draw #t)
(define debug-point-width 0.1)

(define (reset-debug-opts dc)
  (send dc set-brush "white" 'transparent))

; vectors
(define (vec-mult c v)
  (vector-map (λ (x) (* x c)) v))

(define (vec-add va vb)
  (vector-map (λ (a b) (+ a b)) va vb))

(define (vec-sub va vb)
  (vector-map (λ (a b) (- a b)) va vb))

(define (vec-norm v)
  (vec-mult
    (/ 1 (vec-magnitude v)) 
    v))

(define (vec-magnitude v)
  (sqrt
    (foldl + 0
      (map (lambda (x) (sqr x))
        (vector->list v)))))

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

(define (draw-bezier points)
   (draw-curve-points
      (map
         (bezier points)
         (range 0 1 0.001))))



; represents an anchor point in a bezier curve
(struct anchor-point (pos in-vec out-vec))

(define (compound-bezier dc anchor-points)
  (map (lambda (anchors)
    (bezier-from-anchors dc (first anchors) (second anchors)))
    (sliding anchor-points 2)))

; like compound-bezier but the first and last anchors are connected
(define (compound-bezier-loop dc anchor-points)
  (compound-bezier dc (cons (last anchor-points) anchor-points)))

; given a list of vectors representing points attempts to create a smooth
; loop joining all of the points. Returns anchor points representing the 
; resulting compound bezier.
(define (points-to-loop points)
  (map 
    (lambda (trio) (match trio
      [(list p1 p2 p3) 
        (anchor-point 
          p2
          (vec-norm (vec-sub p1 p3))
          (vec-norm (vec-sub p3 p1))
          )]))
  (sliding-loop points 3)))

(define (points-to-loop-bezier dc points)
  (compound-bezier-loop dc (points-to-loop points)))

(define (sliding l n)
  (if (< (length l) n) '()
    (cons (take l n) (sliding (rest l) n))))

(define (sliding-loop l n) 
  (sliding
    (append (take-right l (- n 1)) l)
     n))

; draws a four point bezier from two anchor points
(define (bezier-from-anchors dc a1 a2)
  (if debug-draw (debug-bezier-from-anchors dc a1 a2) '())
  (draw-bezier 
    (list 
      (anchor-point-pos a1)
      (vec-add 
        (anchor-point-pos a1)
        (anchor-point-out-vec a1))
      (vec-add 
        (anchor-point-pos a2)
        (anchor-point-in-vec a2))
      (anchor-point-pos a2))))

(define (debug-bezier-from-anchors dc a1 a2)
    ; control points
    (send dc set-brush "green" 'solid) 
    (let 
      ([a1-control-point-pos 
          (vec-add 
            (anchor-point-pos a1) 
            (anchor-point-out-vec a1))]
        [a2-control-point-pos
          (vec-add 
            (anchor-point-pos a2) 
            (anchor-point-in-vec a2))]
      ) 
        (send dc set-pen "green" 0.01 'solid)
        (send dc draw-line 
          (vector-ref (anchor-point-pos a1) 0)
          (vector-ref (anchor-point-pos a1) 1)
          (vector-ref a1-control-point-pos 0)
          (vector-ref a1-control-point-pos 1))
        (send dc draw-line 
          (vector-ref (anchor-point-pos a2) 0)
          (vector-ref (anchor-point-pos a2) 1)
          (vector-ref a2-control-point-pos 0)
          (vector-ref a2-control-point-pos 1))
        (send dc set-pen "black" 0.01 'solid)
        (debug-point dc a1-control-point-pos)
        (debug-point dc a2-control-point-pos))
    ; anchor points
    (send dc set-brush "red" 'solid) 
    (debug-point dc (anchor-point-pos a1))
    (debug-point dc (anchor-point-pos a2))
    (reset-debug-opts dc))

(define (debug-point dc v)
  (send dc draw-ellipse
      (- (vector-ref v 0) (/ debug-point-width 2))
      (- (vector-ref v 1) (/ debug-point-width 2))
      debug-point-width
      debug-point-width))
