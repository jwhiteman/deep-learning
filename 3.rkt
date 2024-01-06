;; ch 3

(include "lib.rkt")

(define sum-1
  (lambda (t1)
    (sum-helper t1 (sub1 (tlen t1)) 0.0)))

(define sum-helper
  (lambda (t1 idx acc)
    (if (zero? idx)
      (+ acc (tref t1 0))
      (sum-helper t1
                  (sub1 idx)
                  (+ (tref t1 idx) acc)))))


(tensor 3.14 44 -0.9)
(sum-1 (tensor 3.14 44 -0.9))

(sum-1 (tensor (tensor 1 2) (tensor 3 4)))
;; => (tensor 4.0 6.0)  ...how is this working??

(+ (tensor 3.0 4.0) (tensor 1 2))


(tensor (tensor (tensor 1 2 3) (tensor 4 5 6))
        (tensor (tensor 7 8 9) (tensor 10 11 12)))


(sum-1 (tensor (tensor (tensor 1 2 3) (tensor 4 5 6))
               (tensor (tensor 7 8 9) (tensor 10 11 12))))
;; => (tensor (tensor 8.0 10.0 12.0) (tensor 14.0 16.0 18.0))
