#lang racket/base

(provide
 (struct-out tile)
 tile-length
 tile-size
 argb-pixels->tile
 tile->argb-pixels)

(define tile-length 8)
(define tile-size (* tile-length tile-length))

;; Channel offsets in argb bytes
(define alpha-channel 0)
(define red-channel 1)
(define green-channel 2)
(define blue-channel 3)

;; red : Bytes
;; green : Bytes
;; blue : Bytes
;;
;; Stores RGB pixels separated in colour channels. 
(struct tile (red green blue) #:transparent)

(define (argb-pixels->tile pixels)
  (unless (= (bytes-length pixels) (* tile-size 4))
    (raise-type-error 'argb-pixels->tile
                      (format "bytes of length ~a" (* tile-size 4))
                      pixels))
  (define r (make-bytes tile-size))
  (define g (make-bytes tile-size))
  (define b (make-bytes tile-size))
  (for ([i (in-range 0 (* tile-size 4) 4)]
        [j (in-naturals)])
       (bytes-set! r j (bytes-ref pixels (+ i red-channel)))
       (bytes-set! g j (bytes-ref pixels (+ i green-channel)))
       (bytes-set! b j (bytes-ref pixels (+ i blue-channel))))
  (tile r g b))

(define (tile->argb-pixels t)
  (define pixels (make-bytes (* 4 tile-size)))
  (define r (tile-red t))
  (define g (tile-green t))
  (define b (tile-blue t))
  (for ([i (in-range tile-size)]
        [j (in-range 0 (* tile-size 4) 4)])
       (bytes-set! pixels (+ j red-channel) (bytes-ref r i))
       (bytes-set! pixels (+ j green-channel) (bytes-ref g i))
       (bytes-set! pixels (+ j blue-channel) (bytes-ref b i)))
  pixels)