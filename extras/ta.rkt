#lang racket

(require malt)

(define line-xs
  (tensor 2.0 1.0 4.0 3.0))
(define line-ys
  (tensor 1.8 1.2 4.2 3.3))

;; y = mx = b
(define line
  (lambda (xs)
    (lambda (theta)
      (+ (* (list-ref theta 0) xs)
         (list-ref theta 1)))))

;; ((line (tensor 1.0 2.0 3.0)) (list 0.0 0.0))

(define (sum-of-squares ys gs)
  (sum
    (sqr
      (- ys gs))))

(define l2-loss
  (lambda (target loss-f)
    (lambda (xs ys)
      (lambda (theta)
        (loss-f ys ((target xs) theta))))))

(define obj
  ((l2-loss line sum-of-squares) line-xs line-ys))
;; (obj (list 0.0 0.0))

;;(gradient-of obj (list 0.0 0.0))

(define revise
  (lambda (f revs acc)
    (if (zero? revs)
      acc
      (revise f
              (sub1 revs)
              (f acc)))))

;; (revise add1 5 0)

(declare-hyper revs)
(declare-hyper learning-rate)

;; THETAn+1 = THETAn - learning-rate*∇f(THETAn)
(define gradient-descent
  (lambda (obj theta)
    (let ((f (lambda (theta)
               (map (lambda (te ge)
                      (- te (* learning-rate ge)))
                    theta
                    (gradient-of obj theta)))))
      (revise f revs theta))))

(with-hypers
  ((revs 1000)
   (learning-rate 0.01))
  (gradient-descent obj (list 0.0 0.0)))
