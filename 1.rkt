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


(define scaler? number?)

(define rank
  (lambda (t)
    (rank-helper t 0)))

(define rank-helper
  (lambda (t acc)
    (if (scaler? t)
      acc
      (rank-helper (tref t 0)
                   (add1 acc)))))

(define shape
  (lambda (t)
    (reverse (shape-helper t '()))))

(define shape-helper
  (lambda (t acc)
    (if (scaler? t)
      acc
      (shape-helper (tref t 0)
                    (cons (tlen t) acc)))))
