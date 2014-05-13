#lang racket

(require racket/gui/base
         racket/class
         racket/list
         racket/match)

(define b1 (make-bitmap 200 15000))
(define b2 (make-bitmap 200 15000))

(define d1 (new bitmap-dc% [bitmap b1]))
(time (for ([i (in-range 1000)])
        (send d1 draw-bitmap-section b2 0 0 0 0 200 7500)
        (send d1 draw-bitmap-section b2 0 7500 0 7500 200 7500)))
