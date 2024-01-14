#lang racket

(require malt)

(define data-xs (tensor 2.0 1.0 4.0 3.0))
(define data-ys (tensor 1.8 1.2 4.2 3.3))

(define line
  (lambda (x)
    (lambda (theta)
      (+ (* (list-ref theta 0) x)
         (list-ref theta 1)))))

(define l2-loss
  (lambda (target)
    (lambda (xs ys)
      (lambda (theta)
        (let ((guess ((target xs) theta)))
          (sum
            (sqr
              (- ys guess))))))))

(((l2-loss line) data-xs data-ys) (list 0.0 0.0))

(define revise
  (lambda (f revs acc)
    (if (zero? revs)
      acc
      (revise f (sub1 revs) (f acc)))))

(revise
  (lambda (lon)
    (map (lambda (n) (- n 3)) lon))
  5
  '(1 2 3))

(gradient-of sqr (tensor 2 3 4))
(gradient-of (compose sqr sqr) (tensor 2 3))

(gradient-of ((l2-loss line) data-xs data-ys) (list 0.0 0.0))
(gradient-of ((l2-loss line) data-xs data-ys) (list 5.0 5.0))

(define revs 1000)
(define learning-rate 0.01)

;; FINAL LAW OF REVISION:
;; new THETAi = THETAi - (alpha * rate of change of loss w.r.t THETAi)
(define gradient-descent
  (lambda (obj big-theta)
    (let ((f (lambda (big-theta)
               (map (lambda (p g)
                      (- p (* learning-rate g)))
                    big-theta
                    (gradient-of obj big-theta)))))
      (revise f revs big-theta))))

(gradient-descent ((l2-loss line) data-xs data-ys) (list 0.0 0.0))
