;; exponential smoothing - time series forecasting for univariate data...
(define smooth
  (lambda (decay-rate average g)
    (+ (* decay-rate average)
       (* (- 1 decay-rate) g))))
