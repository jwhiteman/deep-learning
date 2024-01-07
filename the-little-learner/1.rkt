#lang racket

(require malt)

(define line-xs
  (tensor 2.0 1.0 4.0 3.0))
(define line-ys
  (tensor 1.8 1.2 4.2 3.3))

(printf "line-xs ~a~%" line-xs)
(printf "line-ys ~a~%" line-ys)

(define line
  (lambda (x)
    (lambda (θ)
      (+ (* (first θ) x) (second θ )))))

(printf "line: ~a~%" ((line 7.3) (list 1.0 0)))
