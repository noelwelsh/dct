#lang racket/base

(require
 (for-syntax racket/base)
 racket/math
 racket/require
 (planet schematics/numeric:1/vector)
 "tile.rkt")

;; Slowest. For debugging and performance testing
;; (require
;;  racket/fixnum
;;  (except-in racket/flonum flvector make-flvector flvector-set! flvector-ref))

;; (define flvector vector)
;; (define make-flvector make-vector)
;; (define flvector-set! vector-set!)
;; (define flvector-ref vector-ref)

;; Medium speed.
;; (require
;;  racket/fixnum
;;  racket/flonum)

;; Fastest
(require
 (only-in racket/flonum make-flvector)
 (filtered-in
  (lambda (name) (regexp-replace #rx"unsafe-" name ""))
  racket/unsafe/ops))


(provide
 basis
 basis-matrices
 dct
 idct
 tile->dct
 dct->tile)

;; One dimensional basis functions 
;(: basis (Vectorof FlVector))
(define basis
  (for/vector ([k tile-length])
    (let ([elt (make-flvector tile-length)])
      (for ([n (in-range tile-length)])
           (flvector-set!
            elt
            n
            (cos (fl*
                  (fl* (fl/ pi (fx->fl tile-length))
                       (fl+ (fx->fl n) .5))
                  (fx->fl k)))))
      elt)))

(define (flvector-outer-product v1 v2)
  (define out (make-flvector tile-size))
  (for* ([i (in-range tile-length)]
         [j (in-range tile-length)])
        (flvector-set! out
                       (fx+ (fx* i tile-length) j)
                       (fl* (flvector-ref v1 i) (flvector-ref v2 j))))
  out)

;; Two dimensional basis functions
;; (: basis-matrices (Vectorof FlVector))
(define basis-matrices
  (for/vector ([k tile-size]
               [i (in-vector basis)]
               #:when #t
               [j (in-vector basis)])
              (flvector-outer-product i j)))

;; (: dct (Bytes -> FlVector))
(define (dct bytes)
  (define accum (make-flvector tile-size 0.0))
  (define out (make-flvector tile-size))
  (for ([i (in-range tile-size)])
       (flvector-set! accum i (fx->fl (bytes-ref bytes i))))
  (for ([basis (in-vector basis-matrices)]
        [n (in-naturals)])
       (flvector-set! out
                      n
                      (fl/
                       (for/fold ([sum 0.0])
                           ([i (in-range tile-size)])
                         (fl+ sum
                              (fl* (flvector-ref accum i)
                                   (flvector-ref basis i))))
                       (fx->fl tile-size))))
  (for* ([i (in-range tile-length)]
         [j (in-range tile-length)])
        (flvector-set! out
                       (fx+ (fx* i tile-length) j)
                       (fl* (flvector-ref out (fx+ (fx* i tile-length) j))
                            (cond
                             [(and (fx= i 0) (fx= j 0)) 1.0]
                             [(or (fx= i 0) (fx= j 0)) 2.0]
                             [else 4.0]))))
  out)

;; (: idct (FlVector -> Bytes))
(define (idct flvector)
  (define (clip b)
    (cond
     [(fx>= b 255) 255]
     [(fx<= b 0) 0]
     [else b]))
  (define bytes (make-bytes tile-size))
  (define accum (make-flvector tile-size 0.0))
  (for ([basis (in-vector basis-matrices)]
        [n (in-range tile-size)])
       (let ([coeff (flvector-ref flvector n)])
         (for ([i (in-range tile-size)])
              (flvector-set!
               accum
               i
               (fl+ (flvector-ref accum i)
                    (fl* coeff (flvector-ref basis i)))))))
  (for ([i (in-range tile-size)])
       (bytes-set! bytes i (clip (fl->fx (flround (flvector-ref accum i))))))
  bytes)

(define (tile->dct t)
  (vector
   (dct (tile-red t))
   (dct (tile-green t))
   (dct (tile-blue t))))

(define (dct->tile d)
  (tile
   (idct (vector-ref d 0))
   (idct (vector-ref d 1))
   (idct (vector-ref d 2))))