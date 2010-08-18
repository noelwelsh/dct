#lang racket/base

(require
 rackunit
 "tile.rkt")

(define (make-random-argb-pixels)
  (define bytes (make-bytes (* tile-size 4)))
  (for ([i (in-range tile-size)])
       (bytes-set! bytes (* i 4) 0)
       (bytes-set! bytes (+ (* i 4) 1) (random 256))
       (bytes-set! bytes (+ (* i 4) 2) (random 256))
       (bytes-set! bytes (+ (* i 4) 3) (random 256)))
  bytes)

(define/provide-test-suite tile-tests

  (test-case
   "tile->argb-pixels and argb-pixels->tile are inverses"
   (for ([i (in-range 1024)])
        (define pixels (make-random-argb-pixels))
        (check-equal?
         (tile->argb-pixels (argb-pixels->tile pixels))
         pixels)))
  )