(require malt/interlude-v) ;; requires the "learner" tensor types

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

;; an example - this is pretty nice
(define add1-ext
  (ext1 add1 0))
(add1-ext (tensor (tensor 1 2 3)
                  (tensor 4 5 6)))

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

;; EXT2

(define rank>
  (lambda (t u)
    (cond ((scaler? t) #f)
          ((scaler? u) #t)
          (else
            (rank> (tref t 0) (tref u 0))))))

(rank> 0 0)                            ;; #f
(rank> (tensor 1) 42)                  ;; #t
(rank> 42 (tensor 1))                  ;; #f
(rank> (tensor (tensor 1)) (tensor 1)) ;; #t

(define of-ranks?
  (lambda (n t m u)
    (and (of-rank? n t)
         (of-rank? m u))))

(of-ranks? 0 0 0 0) ;; #t
(of-ranks? 0 0 1 0) ;; #f

;; 1. t at base rank?   desc into u
;; 2. u at base rank?   desc into t
;; 3. t & u equal rank? desc into both - actually this is length. why?
;; 4. t & u aren't at base rank, and aren't equal so...
;; 4a. t.rank > u.rank? desc into t
;; 4b. else -           desc into u

(define desc-into-u
  (lambda (g t u)
    (tmap (lambda (eu)
            (g t eu)) u)))

(define desc-into-t
  (lambda (g t u)
    (tmap (lambda (et)
            (g et u)) t)))

(define at-base-rank? of-rank?)
(define desc
  (lambda (g n t m u)
    (cond ((at-base-rank? n t) (desc-into-u g t u))
          ((at-base-rank? m u) (desc-into-t g t u))
          ((= (tlen t) (tlen u))
           (tmap g t u))
          ((rank> t u) (desc-into-t g t u))
          (else
            (desc-into-u g t u)))))

(define ext2
  (lambda (f n m)
    (lambda (t u)
      (if (of-ranks? n t m u)
        (f t u)
        (desc (ext2 f n m) n t m u)))))

;; extend +...
(define +-ext
  (ext2 +-0-0 0 0))
(+-ext 2 3)
(+-ext 2 (tensor 2 3 4))
(+-ext (tensor 2 3 4) 3)

;; extend 'my-add'
(define my-add
  (lambda (n k)
    (if (zero? n)
      k
      (my-add (sub1 n)
              (add1 k)))))

(define my-add-ext
  (ext2 my-add 0 0))
(my-add-ext (tensor 1 2 3) 2)
(my-add-ext 2 (tensor 1 2 3))

;; extend modulo
(define modulo-ext
  (ext2 modulo 0 0))
(modulo-ext (tensor 4 4 4) 2)
(modulo-ext 2 (tensor 4 4 4))
(modulo-ext (tensor 7 7 7) (tensor 3 3 3))

;; extend xor
(define bitwise-xor-ext
  (ext2 bitwise-xor 0 0))
(bitwise-xor-ext 4 5)
(bitwise-xor-ext (tensor 3 4 5) 2)
(bitwise-xor-ext 2 (tensor 3 4 5))
(bitwise-xor-ext (tensor 3 4 5) (tensor 3 4 5))

; extend *
(define *-ext
  (ext2 *-0-0 0 0))
(*-ext 2 3)
(*-ext 2 (tensor 2 3 4))
(*-ext (tensor 2 3 4) 2)
(*-ext (tensor 2 3) (tensor 4 5))
(*-ext (tensor 2 3) (tensor 4 5 6))

;; QUESTION 1: how does that ^ work?
;; QUESTION 2: why ask about tlen in desc?
