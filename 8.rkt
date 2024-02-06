(require malt)

;; w/ hyper params: batch-size, α, μ:
;;   SGD: θn+1 = θn - α*∇obj(θn)
;;   MGD: θn+1 = θn - α*∇obj(θn) + μ*θn-1


;; ** inflate **
;; note: e may be a scaler or a tensor
;; something that either returns scaler 0 or a zero'd out tensor
(lambda (e)
  (list e (ZERO e)))

;; deflate
(lambda (p)
  (car p))

;; update
(lambda (p g)
  (let ((v (- (* veloctiy-rate (list-ref p 1))
              (* learning-rate g))))
    (list (+ (list-ref p 0) v) v)))

;; samples
;; revise

;; data-xs
;; data-ys
;; target
;; loss-f
;; l2-loss

;; hp: batch-size
;; hp: revs
;; hp: α
;; hp: μ
;; sampling-obj

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
