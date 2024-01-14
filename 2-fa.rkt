(define line
  (lambda (x)
    (lambda (theta)
      (+ (* (list-ref theta 0) x)
         (list-ref theta 1)))))

;; IDEA 1: with malt, + and times (and maybe more) have been extended to handle
;; tensors.

;; What's it mean for a tensor to be passed to #'line ?
;; Well before, you'd get a scaler back. With a m & b for a given x, you get y.
;; It's almost identical, except that instead of a single, scaler y, you get
;; a tensor of Y. You could say a list of y's. This seems like a convenience.


;; What about rank and shape?

;; RANK: how deeply nested the tensor is. "The number of left brackets until
;; the first scaler. Ex: (tensor (tensor (tensor 1 2) (tensor 3 4))) is 3.

;; I don't want to be in the business of having to look at a complicated,
;; messy, nested tensor and counting them manually. Will just know this:
;; rank: how nested it is. Fine.


;; Shape is more interesting: given a tensor-4, for example, shape will tell you
;; how many tensor-3 are below, and how many tensor-2, how many tensor-1, and
;; how many scaler.

;; This is interesting because while you can easily count the number of elements
;; (members?) in a tensor^n-1, beyond that it's only really convenient (and
;; thankfully only necessary) to look into one of the sub elements. Usually the first.

;; again, note: that it's only tensor^1 that holds the scalers.

;; fucking SUM:
;; #'sum of a tensor-1 is a scaler. it's the same as col.reduce(&:+)
;; #'sum of a tensor-2 is a tensor-1.
;; #'sum of a tensor-3 is a tensor-2, and so on.

;; note that (* n tensor) is still tensor
