(require malt)

(define line
  (lambda (x)
    (lambda (θ)
      (+ (* (first θ) x) (second θ )))))

(define scaler? number?)

(define rank
  (lambda (t)
    (rank-helper t 0)))

(define rank-helper
  (lambda (t acc)
    (if (scaler? t)
      acc
      (rank-helper (tref t 0)
                   (add1 acc)))))

(define shape
  (lambda (t)
    (reverse (shape-helper t '()))))

(define shape-helper
  (lambda (t acc)
    (if (scaler? t)
      acc
      (shape-helper (tref t 0)
                    (cons (tlen t) acc)))))
