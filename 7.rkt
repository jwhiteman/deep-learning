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
  (lambda (number-of-elements limit)
    (letrec ([_ (lambda (i acc)
                  (if (zero? i)
                    acc
                    (_ (sub1 i)
                       (cons (random limit) acc))))])
      (_ number-of-elements '()))))

(define revise
  (lambda (f revs acc)
    (if (zero? revs)
      acc
      (revise f
              (sub1 revs)
              (f acc)))))

(define sum-of-squares
  (lambda (ys ps)
    (sum
      (sqr
        (- ys ps)))))

(define l2-loss
  (lambda (target loss-f)
    (lambda (xs ys)
      (lambda (theta)
        (loss-f ys
                ((target xs) theta))))))

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

;; THETAn+1 = THETAn - learning-rate * ∇f(THETAn)
;; (define gradient-descent
;;   (lambda (obj theta)
;;     (let ((f (lambda (theta)
;;                (map (lambda (te ge)
;;                       (- te (* learning-rate ge)))
;;                     theta
;;                     (gradient-of obj theta)))))
;;       (revise f revs theta))))

(define lonely-i
  (lambda (e) (list e)))

(define lonely-d
  (lambda (l) (list-ref l 0)))

(define lonely-u
  (lambda (p g)
    (list (- (list-ref p 0)
             (* learning-rate g)))))

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

(define lonely-gradient-descent
  (gradient-descent lonely-i lonely-d lonely-u))

;; FLUBS:
;; flub 1: absent mindely used learning-rate instead of theta in gradient-descent
;; flub 2: forgot to wrap lonely-u result in a list
;; flub 3: forgot that lonely-u result 0 needed list-ref
;; flub 4: forgot map in gradient-of deflate

(with-hypers
  ((learning-rate 0.001)
   (revs 15000)
   (batch-size 4))
  (lonely-gradient-descent
    (sampling-obj (l2-loss plane sum-of-squares)
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
  (naked-gradient-descent
    (sampling-obj (l2-loss plane sum-of-squares)
                  plane-xs
                  plane-ys)
                    (list (tensor 0.0 0.0) 0.0)))
