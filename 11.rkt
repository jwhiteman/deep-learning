(require malt)

;; *** LAYER: ***

;; dot-product-2-1
(define dot-product-2-1
  (lambda (n m)
    (sum (*-2-1 n m))))

(dot-product-2-1 (tensor (tensor 2 1 3.1)         ;; n0: t weights
                         (tensor 3.7 4.0 6.1))    ;; n1: t weights
                 (tensor 1.3 0.4 3.3))            ;; t

;; rectify
(define rectify
  (lambda (s)
    (if (< s 0)
      0
      s)))

;; linear
(define linear
  (lambda (t)
    (lambda (theta)
      (+ (dot-product-2-1 t
                          (tref theta 0))
         (tref theta 1)))))

;; relu
(define relu
  (lambda (t)
    (lambda (theta)
      (rectify ((linear t) theta)))))

;; *** NETWORK: ***
;; k-relu
(define k-relu
  (lambda (k)
    (lambda (t)
      (lambda (theta)
        (if (zero? k)
          t
          (((k-relu (sub1 k))
            ((relu t) theta))
           (refr theta 2)))))))
