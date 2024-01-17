#lang racket

(require malt)

(define bmi (tensor 30.30 33.88 31.46 30.21 27.61 32.39 27.91 37.67 39.22 26.74))
(define blood-pressure (tensor 125.45 130.81 127.19 125.32 121.41 128.58 121.86 136.51 138.83 120.12))

;; renaming this to my-line to not conflict w/ plot:
(define my-line
  (lambda (data-xs)
    (lambda (theta)
      (+ (* (list-ref theta 0) data-xs)
         (list-ref theta 1)))))

;; best 0.21 - { revs: 1_000_000, rate: 0.0001 }
(define loss-f-mse
  (lambda (ys guess)
    (/ (sum
         (sqr
           (- ys guess)))
       (tlen ys))))

;; result: 4.8
(define loss-f-abs
  (lambda (ys guess)
    (sum
      (abs
        (- ys guess)))))

;; result: 8.66
(define loss-f-abs-root
  (lambda (ys guess)
    (sqrt (loss-f-abs ys guess))))

;; result: 15.9
(define loss-f-euclid
  (lambda (ys guess)
    (sqrt (loss-f-book ys guess))))

;; result: nan
(define loss-f-book
  (lambda (ys guess)
    (sum
      (sqr
        (- ys guess)))))

;; trying MSE here:
(define loss
  (lambda (target)
    (lambda (data-xs data-ys)
      (lambda (loss-f)
        (lambda (theta)
          (let ((guess ((target data-xs) theta)))
            (loss-f data-ys guess)))))))

(define obj (((loss my-line) bmi blood-pressure) loss-f-mse))

(obj (list 0.0 0.0))
;; 163191.9846

;; this is just reduce, huh?
(define revise
  (lambda (f revs acc)
    (if (zero? revs)
      acc
      (revise f
              (sub1 revs)
              (f acc)))))

;; a quick test:
(revise (lambda (lon)
          (map (lambda (n) (- n 3)) lon))
        5
        '(1 2 3))
;; '(-14 -13 -12)

(define learning-rate 0.0001)
(define revs 1000000)
(define starting-theta (list 0.0 0.0))

(define gradient-descent
  (lambda (obj)
    (revise
      (lambda (theta)
        (map (lambda (te ge)
               (- te (* learning-rate ge)))
             theta
             (gradient-of obj theta)))
      revs
      starting-theta)))

(let ((res (gradient-descent obj)))
  (print (obj (list 0.0 0.0)))
  (newline)
  (print res)
  (newline)
  (print (obj res)))
