(require malt)

;; base "decider" / activation function
;; "non-linear" - i.e no scaling
(define rectify-0
  (lambda (s)
    (if (negative? s)
      0.0
      s)))

(rectify-0 42.0)
(rectify-0 0.0)
(rectify-0 -3.141596)

(define rectify
  (ext1 rectify-0 0))

(rectify (tensor (tensor 3.14 -3.14 0.41)
                 (tensor -2.12 -33 49)))

(dot-product (tensor 1 2 3) (tensor 3 4 5))
;; fyi: works with rank > 1; is that useful? is it still even dot-product at that point?
;; dot-product: vectors...rank > 1 are matrices/tensors?

;; a new target: (fyi - this is plane, but with a new name?)
(define linear-1-1
  (lambda (t)
    (lambda (theta)
      (+ (dot-product (list-ref theta 0)
                      t)
         (list-ref theta 1)))))

;; rectifying linear unit:
(define relu-1-1
  (lambda (t)
    (lambda (theta)
      (rectify ((linear-1-1 t) theta)))))
