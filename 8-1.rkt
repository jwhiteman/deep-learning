(require malt)

;; *** EXPECTANT
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

;; *** OBJ
(define plane-xs
  (tensor (tensor 1.0 2.05)
          (tensor 1.0 3.0)
          (tensor 2.0 2.0)
          (tensor 2.0 3.91)
          (tensor 3.0 6.13)
          (tensor 4.0 8.09)))

(define plane-ys
  (tensor 13.99 15.99 18.0 22.4 30.2 37.94))

;; MISC
(define revise
  (lambda (f revs acc)
    (if (zero? revs)
      acc
      (revise f
              (sub1 revs)
              (f acc)))))

(define samples
  (lambda (num-elements limit)
    (letrec ([_ (lambda (i acc)
                  (if (zero? i)
                    acc
                    (_ (sub1 i)
                       (cons (random limit) acc))))])
      (_ num-elements '()))))

;; HYPERS
(declare-hyper revs)
(declare-hyper learning-rate)
(declare-hyper batch-size)
(declare-hyper momentum-coefficient)

(define sampling-obj
  (lambda (expectant xs ys)
    (let ((limit (tlen xs)))
      (lambda (theta)
        (let ((b (samples batch-size limit)))
          ((expectant (trefs xs b)
                     (trefs ys b)) theta))))))

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

;; *** ATES
(define naked-i (lambda (p) p))
(define velocity-i
  (lambda (p)
    (list p (zeroes p))))

(define naked-d (lambda (p) p))
(define velocity-d
  (lambda (p)
    (car p)))

;; SGD: θn+1 = θn - α∇obj(θn)
;; MGD: θn+1 = θn + V
;;         V = μVn-1 - α∇obj(θn)
(define naked-u
  (lambda (te ge)
    (- te (* learning-rate ge))))
(define velocity-u
  (lambda (p g)
    (let ((v (- (* momentum-coefficient (list-ref p 1))
                (* learning-rate g))))
      (list (+ (list-ref p 0) v) v))))

(define naked-gradient-descent
  (gradient-descent naked-i naked-d naked-u))
(define velocity-gradient-descent
  (gradient-descent velocity-i velocity-d velocity-u))

;; FLUBS:
;; 1. in sampling-obj, forgot to apply the obj function to theta. again.
;; 2. in velocity-d, did αVn-1 instead of α∇obj(θn) (i.e αg)
;; 3. changing an 'ate func w/o re-evaluating (define velocity-gradient-descent ... etc)

;; *** EXPERIMENT
(with-hypers
  ((revs 5000)
   (learning-rate 0.001)
   (batch-size 4)
   (momentum-coefficient 0.9))
  (velocity-gradient-descent
    (sampling-obj (l2-loss plane sum-of-squares) plane-xs plane-ys)
    (list (tensor 0.0 0.0) 0.0)))

;; *** EXPERIMENT
(with-hypers
  ((revs 15000)
   (learning-rate 0.001)
   (batch-size 4)
   (momentum-coefficient 0.9))
  (naked-gradient-descent
    (sampling-obj (l2-loss plane sum-of-squares) plane-xs plane-ys)
    (list (tensor 0.0 0.0) 0.0)))
