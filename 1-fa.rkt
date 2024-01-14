(define xs (tensor 1 2 3 4 5))
(define ys (tensor 2 4 6 8 10))

;; y = mx + b
(define line
  (lambda (x)
    (lambda (theta)
      (+ (* (list-ref theta 0) x)
         (list-ref theta 1)))))

(define l2-loss
  (lambda (target)
    (lambda (xs ys)
      (lambda (theta)
        (let ((pred-ys ((target line) theta)))
          (sum (sqr (- ys pred-ys)))))))

;; 1: talk through this. is it correct?
;; 2: understand how the chapter demonstrates
;;   - rate-of-change
;;   - learning-rate
;;   - relationship to derivatives
