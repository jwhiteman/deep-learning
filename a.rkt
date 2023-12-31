#lang racket

;; there is a conflict between these two libs, unfortunately...
(require malt)
(require rackunit)

(define (add a b)
  (+ a b))

(check-equal? (add 1 2) 3 "addition of 1 and 2 should be 3")
