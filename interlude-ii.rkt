#lang racket

(require malt)

(define xs (tensor 2.0 1.0 4.0 3.0))
(define ys (tensor 1.8 1.2 4.2 3.3))

(define line
  (lambda (inputs)
    (lambda (theta)
      (+ (* (list-ref theta 0) inputs)
         (list-ref theta 1)))))

(define sum-of-squares
  (lambda (expected actual)
    (sum
      (sqr
        (- expected actual)))))

(define (mse expected actual)
    (/ (sum-of-squares expected actual) (tlen actual)))

(define l2-loss
  (lambda (target loss-f)
    (lambda (inputs outputs)
      (lambda (theta)
        (let ((guess ((target inputs) theta)))
          (loss-f outputs guess))))))

(define obj ((l2-loss line sum-of-squares) xs ys))

(define revise
  (lambda (f revs acc)
    (if (zero? revs)
      acc
      (revise f
              (sub1 revs)
              (f acc)))))

(declare-hyper learning-rate)
(declare-hyper revs)

;; theta-1 = theta-0 - (learning-rate * gradient-of(theta-0))
(define gradient-descent
  (lambda (obj)
    (revise (lambda (theta)
              (map (lambda (te ge)
                     (- te (* learning-rate ge)))
                   theta
                   (gradient-of obj theta)))
            revs
            (list 0.0 0.0))))

(with-hypers ((learning-rate 0.01)
              (revs 1000))
             (gradient-descent obj))
