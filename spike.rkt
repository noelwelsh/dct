#lang racket/gui

(require (planet schematics/numeric:1/vector))

(define texture
  (make-object bitmap%
               "floorboards-small.jpg"
               'jpeg))


;; Length in pixels of a tile square
(define tile-length 8)

;; Type tile: bytes
;; Type dct : (Vector #f (Vectorof Flonum) (Vectorof Flonum) (Vectorof Flonum))

(define (make-tile)
  (make-bytes (* tile-length tile-length 4)))

(define (texture-get-tile x y)
  (define buffer (make-tile))
  (send texture get-argb-pixels x y tile-length tile-length buffer)
  buffer)


;; Channels
(define alpha 0)
(define red 1)
(define green 2)
(define blue 3)

;; tile -> byte
(define (tile-ref tile x y channel)
  (bytes-ref tile (+ (* y tile-length 4) (* x 4) channel)))

(define (tile-set! tile x y channel val)
  (bytes-set! tile (+ (* y tile-length 4) (* x 4) channel) val))


(define (tile->dct tile)
  (define red-dct (tile->dct-channel tile red))
  (define green-dct (tile->dct-channel tile green))
  (define blue-dct (tile->dct-channel tile blue))
  (vector #f red-dct green-dct blue-dct))

(define pi/8 (/ pi 8))
(define root-1/4 (sqrt 1/4))
(define root-1/8 (sqrt 1/8))
(define root-2/8 (sqrt 2/8))

(define (tile->dct-channel tile channel)
  (define (normalise weight u v)
    (* weight
       (if (zero? u) root-1/8 root-2/8)
       (if (zero? v) root-1/8 root-2/8)))
  (for/vector ([offset (* tile-length tile-length)]
               [u (in-range tile-length)]
               #:when #t
               [v (in-range tile-length)])
    (normalise
     (for/fold ([weight 0])
         ([x (in-range tile-length)]
          #:when #t
          [y (in-range tile-length)])
       (+ weight
          (* (- (tile-ref tile x y channel) 128)
             (cos (* pi/8 u (+ x 1/2)))
             (cos (* pi/8 v (+ y 1/2))))))
     u v)))

(define (dct->tile dct)
  (define tile (make-tile))
  (dct-channel->tile! (vector-ref dct red) red tile)
  (dct-channel->tile! (vector-ref dct green) green tile)
  (dct-channel->tile! (vector-ref dct blue) blue tile)
  tile)

(define (dct-channel->tile! dct-channel channel tile)
  (define (normalise weight u v)
    (define val
      (+
       (* weight
          (if (zero? u) root-1/4 root-2/8)
          (if (zero? v) root-1/4 root-2/8))
       128))
    (cond
      [(> val 255) 255]
      [(< val 0) 0]
      [else
       (inexact->exact (round val))]))
  (for ([x (in-range tile-length)]
        #:when #t
        [y (in-range tile-length)])
    (tile-set!
     tile
     x
     y
     channel
     (normalise
      (for/fold ([weight 0])
        ([u (in-range tile-length)]
         #:when #t
         [v (in-range tile-length)])
        (if (and (zero? u) (zero? v))
            (/ (vector-ref dct-channel (+ (* v tile-length) u)) 2)
            (+ weight
               (* (vector-ref dct-channel (+ (* v tile-length) u))
                  (cos (* pi/8 u (+ x 1/2)))
                  (cos (* pi/8 v (+ y 1/2)))))))
        x y))))

(define the-tile (texture-get-tile 0 0))
(equal? the-tile (dct->tile (tile->dct the-tile)))

(define (display-tile tile)
  (for ([b (in-bytes tile)])
       (display b) (display " ")))

(define (display-bitmap bitmap)
  (define f
    (new frame%
         [label "Bitmap"]
         [width (send bitmap get-width)]
         [height (send bitmap get-height)]))
  (define c (new canvas%
                 [parent f]
                 [paint-callback
                  (lambda (canvas dc)
                    (send dc draw-bitmap bitmap 0 0))]))

  (send f show #t)
  f)

(define fucked-texture
  (let* ([bitmap (make-object
                     bitmap%
                   "floorboards-small.jpg"
                   'jpeg)]
         [dc (new bitmap-dc% [bitmap bitmap])])
    (for ([x (in-range 0 (send bitmap get-width) tile-length)]
          #:when #t
          [y (in-range 0 (send bitmap get-height) tile-length)])
         (send dc set-argb-pixels x y tile-length tile-length
               (dct->tile (tile->dct (texture-get-tile x y)))))
    bitmap))
