#lang racket/base

(require
 racket/flonum
 rackunit
 "dct.rkt"
 "tile.rkt"
 "util.rkt")

(define (flvector->bytes flv)
  (define bytes (make-bytes (flvector-length flv)))
  (for ([i (in-range (flvector-length flv))])
       (bytes-set! bytes
                   i
                   (inexact->exact (round (+ 128 (* 127 (flvector-ref flv i)))))))
  bytes)

(define (flvector->vector flv)
  (define out (make-vector (flvector-length flv)))
  (for ([i (in-range (flvector-length flv))])
       (vector-set! out i (flvector-ref flv i)))
  out)

(define-check (check-dct-invertable bytes)
  (let* ([d (dct bytes)]
         [b (idct d)])
    (for ([b1 (in-bytes bytes)]
          [b2 (in-bytes b)]
          [i (in-naturals)])
         (if (> (abs (- b1 b2)) 0)
             (fail
              (format "Difference between byte ~a and ~a at idx ~a exceeds threshold"
                      b1 b2 i))
             #t))))

(define/provide-test-suite dct-tests
 (test-case
  "DCT and IDCT are inverses for all zero tile"
  (check-dct-invertable
   (make-bytes tile-size)))

 (test-case
  "DCT and IDCT are inverses for constant tile"
  (for ([i (in-range 1024)])
      (let ([bytes (make-bytes tile-size (random 255))])
        (check-dct-invertable bytes))))

 (test-case
  "DCT and IDCT are inverses for basis matrices"
  (for ([basis (in-vector basis-matrices)]
        [i (in-naturals)])
       (define bytes (flvector->bytes basis))
       (with-check-info
        (['message (format "Basis matrix ~a" i)])
          (check-dct-invertable bytes))))

 (test-case
  "DCT and IDCT are inverses for random bytes"
 (for ([i (in-range 1024)])
      (let ([bytes (make-random-bytes)])
        (check-dct-invertable bytes))))
 )

