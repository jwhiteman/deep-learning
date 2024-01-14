#lang racket

(require malt)

;; TODO: try to get this to work
;; TODO: plot everything you can

(define bmi (tensor 30.30 33.88 31.46 30.21 27.61 32.39 27.91 37.67 39.22 26.74))
(define blood-pressure (tensor 125.45 130.81 127.19 125.32 121.41 128.58 121.86 136.51 138.83 120.12))

;(require malt)
;(define data-xs (tensor 2.0 1.0 4.0 3.0))
;(define data-ys (tensor 1.8 1.2 4.2 3.3))
;(define bmi data-xs)
;(define blood-pressure data-ys)

(define line
  (lambda (data-xs)
    (lambda (theta)
      (+ (* (list-ref theta 0) data-xs)
         (list-ref theta 1)))))


;; TODO: try Mean Squared Error
(define loss
  (lambda (target)
    (lambda (data-xs data-ys)
      (lambda (theta)
        (let ((guess ((target data-xs) theta)))
          (sqrt
            (sum
              (sqr
                (- data-ys guess)))))))))

(define obj ((loss line) bmi blood-pressure))

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

(define learning-rate 0.001)
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
  (print res)
  (newline)
  (print (obj res)))

;; BEST: 0.001 & 1_000_000 revs
;;'(1.5504921323393905 80.00366215346878)
;; -> loss of 5 something

;; revs.times.reduce([0.0, 0.0]) do |theta, _n|
;;   theta - alpha * gradient-of(obj, theta)
;; end
;;
;; revs.times.reduce([0.0, 0.0]) do |theta, _n|
;;   gradient-of(obj, theta).each_with_index.map |e, idx|
;;     theta[idx] - alpha * e
;;   end
;; end
