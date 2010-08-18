#lang racket/base

(require
 rackunit
 rackunit/text-ui
 "dct-test.rkt"
 "tile-test.rkt")

(define/provide-test-suite all-tests
  tile-tests
  dct-tests)

(run-tests all-tests)