(require malt)

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
;; *** END PLANE ***

(define samples
  (lambda (num-elements limit)
    (letrec ([_ (lambda (i acc)
                  (if (zero? i)
                    acc
                    (_ (sub1 i)
                       (cons (random limit) acc))))])
      (_ num-elements '()))))

(define revise
  (lambda (f revs acc)
    (if (zero? revs)
      acc
      (revise f
              (sub1 revs)
              (f acc)))))

(define sum-of-squares
  (lambda (te ge)
    (sum
      (sqr
        (- te ge)))))

(define l2-loss
  (lambda (target loss-f)
    (lambda (xs ys) ;; the expectant function
      (lambda (theta)
        (loss-f ys
                ((target xs) theta))))))

;; HYPERS:
(declare-hyper revs)
(declare-hyper learning-rate)
(declare-hyper batch-size)

(define sampling-obj
  (lambda (expectant xs ys)
    (let ((limit (tlen xs)))
      (lambda (theta)
        (let ((b (samples batch-size limit)))
          ((expectant (trefs xs b)
                      (trefs ys b)) theta))))))


(sampling-obj (l2-loss plane sum-of-squares) plane-xs plane-ys)

;; THETAn+1 = THETAn - learning-rate * ∇obj(THETAn)
(define gradient-descent
  (lambda (obj theta)
    (let ((f (lambda (theta)
               (map (lambda (te ge)
                      (- te (* learning-rate ge)))
                    theta
                    (gradient-of obj theta)))))
      (revise f revs theta))))

(with-hypers
  ((learning-rate 0.001)
   (revs 15000)
   (batch-size 4))
  (gradient-descent (sampling-obj
                      (l2-loss plane sum-of-squares)
                      plane-xs
                      plane-ys)
                    (list (tensor 0.0 0.0) 0.0)))

;; gradient descent: new
(define gradient-descent
  (lambda (inflate deflate update)
    (lambda (obj theta)
      (let ((f (lambda (theta)
                 (map update
                      theta
                      (gradient-of obj (map deflate theta))))))
        (map deflate
             (revise f revs
                     (map inflate theta)))))))

(define lonely-i
  (lambda (e)
    (list e)))

(define lonely-d
  (lambda (e)
    (car e)))

(define lonely-u
  (lambda (te ge)
    (list (- (list-ref te 0)
             (* learning-rate ge)))))

(define lonely-gradient-descent
  (gradient-descent lonely-i lonely-d lonely-u))

(with-hypers
  ((learning-rate 0.001)
   (revs 15000)
   (batch-size 4))
  (lonely-gradient-descent (sampling-obj
                             (l2-loss plane sum-of-squares)
                             plane-xs
                             plane-ys)
                           (list (tensor 0.0 0.0) 0.0)))

(define naked-i
  (lambda (e) e))
(define naked-d
  (lambda (e) e))
(define naked-u
  (lambda (te ge)
    (- te (* learning-rate ge))))

(define naked-gradient-descent
  (gradient-descent naked-i naked-d naked-u))

(with-hypers
  ((learning-rate 0.001)
   (revs 15000)
   (batch-size 4))
  (naked-gradient-descent (sampling-obj
                            (l2-loss plane sum-of-squares)
                            plane-xs
                            plane-ys)
                          (list (tensor 0.0 0.0) 0.0)))

