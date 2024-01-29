(require malt)

;; *** PLANE ***:
(define plane-xs
  (tensor (tensor 1.0 2.05)
          (tensor 1.0 3.0)
          (tensor 2.0 2.0)
          (tensor 2.0 3.91)
          (tensor 3.0 6.13)
          (tensor 4.0 8.09))) ;; (6, 2)

(define plane-ys
  (tensor 13.99 15.99 18.0 22.4 30.2 37.94))

(define plane
  (lambda (t)
    (lambda (theta)
      (+ (dot-product t
                      (list-ref theta 0))
         (list-ref theta 1)))))

(define sum-of-squares
  (lambda (ys pred-ys)
    (sum
      (sqr
        (- ys pred-ys)))))

(define l2-loss
  (lambda (target loss-f)
    (lambda (xs ys)
      (lambda (theta)
        (loss-f ys
                ((target xs) theta))))))

(define revise
  (lambda (f revs acc)
    (if (zero? revs)
      acc
      (revise f
              (sub1 revs)
              (f acc)))))

;; SAMPLES!
(define samples
  (lambda (num-elements limit)
    (letrec ([_ (lambda (idx acc)
                  (if (zero? idx)
                    acc
                    (_ (sub1 idx)
                       (cons (random limit) acc))))])
      (_ num-elements '()))))

;; SAMPLING-OBJ
(define sampling-obj
  (lambda (expectant xs ys)
    (let ((limit (tlen xs)))
      (lambda (theta)
        (let ((b (samples batch-size limit)))
          ((expectant (trefs xs b)
                      (trefs ys b)) theta))))))

(declare-hyper learning-rate)
(declare-hyper revs)
(declare-hyper batch-size)

;; THETAn+1 = THETAn - learning-rate * DELf(THETAn)
(define gradient-descent
  (lambda (obj theta)
    (let ((f (lambda (theta)
               (map (lambda (te ge)
                      (- te (* learning-rate ge)))
                    theta
                    (gradient-of obj theta)))))
      (revise f revs theta))))

(define obj ((l2-loss plane sum-of-squares) plane-xs plane-ys))

(with-hypers
  ((learning-rate 0.001)
   (revs 15000)
   (batch-size 4))
  (gradient-descent
    (sampling-obj (l2-loss plane sum-of-squares) plane-xs plane-ys)
      (list (tensor 0.0 0.0) 0.0)))
