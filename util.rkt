#lang racket/base

(require
 "tile.rkt")

(provide
 make-random-bytes)

(define (make-random-bytes)
  (define bytes (make-bytes tile-size))
  (for ([i (in-range tile-size)])
       (bytes-set! bytes i (random 256)))
  bytes)
