#lang racket/gui

(require
 "tile.rkt"
 "dct.rkt")

(define texture
  (make-object bitmap%
               "cauliflower.jpg"
               'jpeg))

(define (bitmap-get-tile b x y)
  (define pixels (make-bytes (* 4 tile-size)))
  (send b get-argb-pixels x y tile-length tile-length pixels)
  (argb-pixels->tile pixels))

(define reconstructed-texture
  (let* ([bitmap (make-object
                     bitmap%
                   "cauliflower.jpg"
                   'jpeg)]
         [dc (new bitmap-dc% [bitmap bitmap])])
    (for ([x (in-range 0 (send bitmap get-width) tile-length)]
          #:when #t
          [y (in-range 0 (send bitmap get-height) tile-length)])
         (send dc set-argb-pixels x y tile-length tile-length
               (tile->argb-pixels (dct->tile (tile->dct (bitmap-get-tile texture x y))))))
    bitmap))

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