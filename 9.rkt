(require malt)

;; MISC
(define revise
  (lambda (f revs acc)
    (if (zero? revs)
      acc
      (revise f
              (sub1 revs)
              (f acc)))))

(define samples
  (lambda (num-elements limit)
    (letrec ([_ (lambda (i acc)
                  (if (zero? i)
                    acc
                    (_ (sub1 i)
                       (cons (random limit) acc))))])
      (_ num-elements '()))))

(define smooth
  (lambda (decay-rate avg g)
    (+ (* decay-rate avg)
       (* (- 1 decay-rate) g))))

;; OBJ
(define plane-xs
  (tensor (tensor 1.0 2.05)
          (tensor 1.0 3.0)
          (tensor 2.0 2.0)
          (tensor 2.0 3.91)
          (tensor 3.0 6.13)
          (tensor 4.0 8.09)))

(define plane-ys
  (tensor 13.99 15.99 18.0 22.4 30.2 37.94))

;; EXPECTANT
(define plane
  (lambda (t)
    (lambda (theta)
      (+ (dot-product t
                      (list-ref theta 0))
         (list-ref theta 1)))))

(define sum-of-squares
  (lambda (ys predicted-ys)
    (sum
      (sqr
        (- ys predicted-ys)))))

(define l2-loss
  (lambda (target loss-f)
    (lambda (xs ys)
      (lambda (theta)
        (loss-f ys
                ((target xs) theta))))))

;; HYPERS
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

(define obj
  ((l2-loss plane sum-of-squares) plane-xs plane-ys))

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

;; W/ "NAKED" GRADIENT DESCENT:
(define naked-i
  (lambda (p) p))
(define naked-d
  (lambda (p) p))
(define naked-u
  (lambda (p g)
    (- p (* learning-rate g))))
(define naked-gradient-descent
  (gradient-descent naked-i naked-d naked-u))

(with-hypers
  ((revs 5000)
   (learning-rate 0.001)
   (batch-size 4))
  (naked-gradient-descent
    (sampling-obj (l2-loss plane sum-of-squares)
                  plane-xs
                  plane-ys)
    (list (tensor 0.0 0.0) 0.0)))

(declare-hyper momentum-coefficient)

;; W/ MOMENTUM GRADIENT DESCENT:
(define velocity-i
  (lambda (p)
    (list p (zeroes p))))
(define velocity-d
  (lambda (p)
    (car p)))
(define velocity-u
  (lambda (p g)  ;; ([Pn, Vn], g)
    (let ((v (- (* momentum-coefficient (list-ref p 1))
                (* learning-rate g))))
      (list (+ (list-ref p 0) v) v))))
(define momentum-gradient-descent
  (gradient-descent velocity-i velocity-d velocity-u))

(with-hypers
  ((revs 5000)
   (learning-rate 0.001)
   (batch-size 4)
   (momentum-coefficient 0.9))
  (momentum-gradient-descent
    (sampling-obj (l2-loss plane sum-of-squares)
                  plane-xs
                  plane-ys)
    (list (tensor 0.0 0.0) 0.0)))


;; W/ RMSprop GRADIENT DESCENT:
(declare-hyper decay-rate)
(declare-hyper epsilon)

(define rms-i
  (lambda (p)
    (list p (zeroes p))))
(define rms-d
  (lambda (p)
    (car p)))
(define rms-u
  (lambda (p g) ;; ([Pn, Rn], g)
    (let* ((r (smooth decay-rate (list-ref p 1) (sqr g)))
           (a (/ learning-rate (+ (sqrt r) epsilon))))
      (list (- (list-ref p 0) (* a g))
            r))))
(define rms-gradient-descent
  (gradient-descent rms-i rms-d rms-u))

(with-hypers
  ((revs 3000)
   (learning-rate 0.01)
   (batch-size 4)
   (decay-rate 0.9)
   (epsilon 1e-8))
  (rms-gradient-descent
    (sampling-obj (l2-loss plane sum-of-squares)
                  plane-xs
                  plane-ys)
    (list (tensor 0.0 0.0) 0.0)))

;; W/ ADAM GRADIENT DESCENT:
(define adam-i
  (lambda (p)
    (list p (zeroes p) (zeroes p))))
(define adam-d
  (lambda (p)
    (car p)))
(define adam-u
  (lambda (p g) ;; ([Pn Vn Rn], g)
    (let* ((r (smooth decay-rate (list-ref p 2) (sqr g)))
           (a (/ learning-rate (+ (sqrt r) epsilon)))
           (v (smooth momentum-coefficient (list-ref p 1) g)))
      (list (- (list-ref p 0) (* a v))
            v
            r))))
(define adam-gradient-descent
  (gradient-descent adam-i adam-d adam-u))

(with-hypers
  ((revs 1500)
   (learning-rate 0.01) ;; altered as per errata
   (batch-size 4)
   (decay-rate 0.9)
   (momentum-coefficient 0.85)
   (epsilon 1e-8))
  (adam-gradient-descent
    (sampling-obj (l2-loss plane sum-of-squares)
                  plane-xs
                  plane-ys)
    (list (tensor 0.0 0.0) 0.0)))

