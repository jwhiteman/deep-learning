#lang racket

(require plot)
(plot-new-window? #t)

(define bmi (list 30.30 33.88 31.46 30.21 27.61 32.39 27.91 37.67 39.22 26.74))
(define blood-pressure (list 125.45 130.81 127.19 125.32 121.41 128.58 121.86 136.51 138.83 120.12))

(define x-values bmi)
(define y-values blood-pressure)

(define m 1.61464)
(define b 76.304)

(plot (list (axes)
            (function (lambda (x) (+ (* m x) b)) (apply min x-values) (apply max x-values))
            (points (map vector x-values y-values))))
