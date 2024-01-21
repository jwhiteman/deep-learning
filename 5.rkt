#lang racket

(require malt)

(define line-xs
  (tensor 2.0 1.0 4.0 3.0))
(define line-ys
  (tensor 1.8 1.2 4.2 3.3))

;; y = mx + b
(define line
  (lambda (inputs)
    (lambda (theta)
      (+ (* (list-ref theta 0) inputs)
         (list-ref theta 1)))))

(define sum-of-squares
  (lambda (real-ys guess-ys)
    (sum
      (sqr
        (- real-ys guess-ys)))))

(define l2-loss
  (lambda (target loss-f)
    (lambda (xs ys)
      (lambda (theta)
        (loss-f ys ((target xs) theta))))))

(define obj ((l2-loss line sum-of-squares) line-xs line-ys))

(define revise
  (lambda (f revs acc)
    (if (zero? revs)
      acc
      (revise f
              (sub1 revs)
              (f acc)))))

(declare-hyper revs)
(declare-hyper learning-rate)

;; THETAn+1 = THETAn - (learning-rate * ∇f(THETAn)

(define gradient-descent
  (lambda (obj initial)
    (revise
      (lambda (theta)
        (map (lambda (te ge)
               (- te (* learning-rate ge)))
             theta
             (gradient-of obj theta)))
      revs
      initial)))

(with-hypers ((revs 1000)
              (learning-rate 0.01))
             (gradient-descent obj (list 0.0 0.0)))

;; *** QUAD ***:
(define quad-xs (tensor -1.0 0.0 1.0 2.0 3.0))
(define quad-ys (tensor 2.55 2.1 4.35 10.2 18.25))

;; ax^2 + bx + c
(define quad
  (lambda (xs)
    (lambda (theta)
      (+ (* (list-ref theta 0)
            (sqr xs))
         (+ (* (list-ref theta 1) xs)
            (list-ref theta 2))))))

(define obj2 ((l2-loss quad sum-of-squares) quad-xs quad-ys))

(obj2 (list 0.0 0.0 0.0))

(with-hypers ((revs 1000)
              (learning-rate 0.001))
             (gradient-descent obj2 (list 0.0 0.0 0.0)))
;; '(1.4787394427094362 0.9928606519360353 2.0546423148479684)

;; *** PLANE ***:
(define plane-xs
  (tensor (tensor 1.0 2.05)
          (tensor 1.0 3.0)
          (tensor 2.0 2.0)
          (tensor 2.0 3.91)
          (tensor 3.0 6.13)
          (tensor 4.0 8.09)))

(define plane-ys
  (tensor 13.99 15.99 18.0 22.4 30.2 37.94))

(define plane
  (lambda (t)
    (lambda (theta)
      (+ (dot-product t
                      (list-ref theta 0))
         (list-ref theta 1)))))

(define obj3 ((l2-loss plane sum-of-squares) plane-xs plane-ys))

(with-hypers ((revs 1000)
              (learning-rate 0.001))
             (gradient-descent obj3 (list (tensor 0.0 0.0) 0.0)))

'((tensor 3.9775764460906315 2.0496557321494446) 5.786758464448077)
