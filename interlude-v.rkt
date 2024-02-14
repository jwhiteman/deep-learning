(require malt/interlude-v) ;; requires the 'learner' tensor types

;; MISC
(sqrt-0 2) ;; note usage
(define scaler? number?) ;; couldn't find this defined in malt, so...

;; tmap
(tmap add1 (tensor 1 2 3))
(tmap + (tensor 1 2 3) (tensor 1 2 3))

;; of-rank? - obvious
(define of-rank?
  (lambda (n t)
    (= n (rank t))))

;; of-rank? - fun
(define of-rank?
  (lambda (n t)
    (cond ((zero? n) (scaler? t))
          ((scaler? t) #f)
          (else
            (of-rank? (sub1 n)
                      (tref t 0))))))

;; sqrt v1
(define sqrt
  (lambda (t)
    (if (of-rank? 0 t)
      (sqrt-0 t)
      (tmap sqrt t))))

(sqrt 2)
(sqrt (tensor (tensor 2 2 2) (tensor 3 3 3)))

;; ext1
(define ext1
  (lambda (f n)
    (lambda (t)
      (if (of-rank? n t)
        (f t)
        (tmap (ext1 f n) t)))))

(define sqrt (ext1 sqrt-0 0))
(define zeroes (ext1 (lambda (x) 0.0) 0))

(zeroes 3.1415)
(zeroes (tensor (tensor (tensor 3.14 3.14 3.14))))

;; sum [sum-1]
(define sum (ext1 sum-1 1))
(sum (tensor (tensor (tensor 1 2) (tensor 2 3))
             (tensor (tensor 3 4) (tensor 5 6))))

;; flatten [flatten-2]
(flatten-2 (tensor (tensor 1 2) (tensor 3 4)))
(define flatten (ext1 flatten-2 2))
(flatten (tensor (tensor 1 2) (tensor 3 4)))



;; EXT2:
;; rank> v1
;; rank> v2
;; of-ranks
;; desc
;; desc-t
;; desc-ukj:w
;; ext2
;; +
;; *
;; sqr
;; dot (dot-1,1 -> ?)
;; *^2,1
