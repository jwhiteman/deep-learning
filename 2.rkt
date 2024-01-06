(require malt)

(define (scaler? n) (number? n))

;; *** RANK: normal
(define rank
  (lambda (t)
    (rank-helper t 0)))

(define rank-helper
  (lambda (t acc)
    (if (scaler? t)
      acc
      (rank-helper (tref t 0)
                   (add1 acc)))))

;; *** RANK: w/ letrec
(define (rank t)
  (letrec ((_ (lambda (t acc)
                (if (scaler? t)
                  acc
                  (_ (tref t 0)
                     (add1 acc))))))
    (_ t 0)))

(rank (tensor (tensor (tensor 1 2))))
;; => 3

;; *** SHAPE: normal
(define shape
  (lambda (t)
    (if (scaler? t)
      '()
      (cons (tlen t)
            (shape (tref t 0))))))

(shape (tensor (tensor 1 2 3) (tensor 4 5 6)))

;; *** SHAPE: w/ accumulator
(define shape
  (lambda (t)
    (reverse (shape-helper t '()))))

(define shape-helper
  (lambda (t acc)
    (if (scaler? t)
      acc
      (shape-helper (tref t 0)
                    (cons (tlen t) acc)))))

(shape (tensor (tensor 1 2 3) (tensor 4 5 6)))
;; => '(2 3)
