#lang racket/base

(require
 profile
 "tile.rkt"
 "dct.rkt"
 "util.rkt")

;; Make runs reproducible
(random-seed 22457)

(define data
  (for/list ([i (in-range 1024)])
            (make-random-bytes)))

(time
 (for ([b (in-list data)])
      (idct (dct b))))

(printf "Calculating DCTS\n")
(define dcts
  (time
   (for/list ([b (in-list data)])
             (dct b))))

(printf "Calculating IDCTS\n")
(define idcts
  (time
   (for/list ([d (in-list dcts)])
             (idct d))))

;; (profile-thunk
;;  (lambda ()
;;    (for ([b (in-list data)])
;;         (idct (dct b)))))

;; Some results
;; dct.rkt with slowest settings
;; Seg fault
;; flround -> round
;; cpu time: 273 real time: 274 gc time: 31
;; Calculating DCTS
;; cpu time: 133 real time: 135 gc time: 12
;; Calculating IDCTS
;; cpu time: 128 real time: 130 gc time: 7

;; dct.rkt with medium settings
;; Seg fault
;; flround -> round
;; cpu time: 184 real time: 185 gc time: 16
;; Calculating DCTS
;; cpu time: 103 real time: 106 gc time: 8
;; Calculating IDCTS
;; cpu time: 72 real time: 75 gc time: 0

;; dct.rkt with fastest settings
;; cpu time: 170 real time: 171 gc time: 30
;; Calculating DCTS
;; cpu time: 102 real time: 103 gc time: 7
;; Calculating IDCTS
;; cpu time: 46 real time: 45 gc time: 0

