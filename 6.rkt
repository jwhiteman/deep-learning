(require malt)

;; samples
(define samples
  (lambda (num-elements limit)
    (letrec ([_ (lambda (i acc)
                  (if (zero? i)
                    acc
                    (_ (sub1 i)
                       (cons (random limit) acc))))])
      (_ num-elements '()))))

;; trefs
(define some-tensor
  (tensor (tensor 5 2 1)
          (tensor 3 8 7)
          (tensor 2 6 -4)
          (tensor 9 8 1)
          (tensor 2 -2 0)
          (tensor 6 6 6)
          (tensor 2 -2 2))) ;; (7, 3)

(trefs some-tensor (samples 2 7))

;; loss
(define (sum-of-squares ys gs)
  (sum
    (sqr
      (- ys gs))))

(define l2-loss
  (lambda (target loss-f)
    (lambda (xs ys)
      (lambda (theta)
        (loss-f ys ((target xs) theta))))))

;; revise
(define revise
  (lambda (f revs acc)
    (if (zero? revs)
      acc
      (revise f
              (sub1 revs)
              (f acc)))))

(declare-hyper learning-rate)
(declare-hyper revs)
(declare-hyper batch-size)

;; gradient-descent
;; THETAn+1 = THETAn - alpha * DELf(THETAn)
(define gradient-descent
  (lambda (obj theta)
    (let ((f (lambda (theta)
               (map (lambda (te ge)
                      (- te (* learning-rate ge)))
                      theta
                      (gradient-of obj theta)))))
          (revise f revs theta))))

;; sampling-obj
(define sampling-obj
  (lambda (expectant xs ys)
    (let ((limit (tlen xs)))
      (lambda (theta)
        (let ((b (samples batch-size limit)))
          ((expectant (trefs xs b) (trefs ys b)) theta))))))

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

(with-hypers
  ((revs 15000)
   (learning-rate 0.001)
   (batch-size 4))
  (gradient-descent
    (sampling-obj
      (l2-loss plane sum-of-squares) plane-xs plane-ys)
    (list (tensor 0.0 0.0) 0.0)))
