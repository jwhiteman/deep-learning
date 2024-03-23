#lang racket
(require malt)
(require malt/examples/morse)

;; linear portion of cnn layer function
(define corr
  (lambda (t)
    (lambda (theta)
      (+ (correlate t (list-ref theta 0)) ;; filter bank
         (list-ref theta 1)))))           ;; bias

(define recu
  (lambda (t)
    (lambda (theta)
      (rectify ((corr t) theta)))))

(define recu-block
  (lambda (b m d)
    (block recu
           (list
             (list b m d) ;; b: number of filters
             (list b))))) ;; ...analogous to number of neurons

(define sum-2 sum-1)
(define sum-cols
  (ext1 sum-2 2))

(define skip
  (λ (f j)
     (λ (t)
        (λ (θ)
           (+ ((f t) θ)
              (correlate θj t))))))
