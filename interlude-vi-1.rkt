#lang racket
(require malt)
(require malt/examples/iris)

(define dense-block
  (lambda (n m)
    (block relu
           (list (list n m)
                 (list n)))))

;; iris-network
(define iris-network
  (stack-blocks
    (list
      (dense-block 6 4)
      (dense-block 3 6))))

;; init-shape
(define init-shape
  (lambda (s)
    (cond
      ((= 1 (len s)) (zero-tensor s))
      ((= 2 (len s))
       (random-tensor 0.0 (/ 2 (list-ref s 1)) s))))) ;; idx 1 encodes the width; no need for len here...

(random-tensor 0.0 0.5 '(6 4))
(init-shape '(4))
(init-shape '(6 4))

;; init-theta
(define init-theta
  (lambda (shape-list)
    (map init-shape shape-list)))

;; iris-classifier
(define iris-classifier (block-fn iris-network))
;; iris-theta-shapes
(define iris-theta-shapes (block-ls iris-network))

(init-theta iris-theta-shapes)

;; iris-theta
(define iris-theta
  (with-hypers
    ((revs 2000)
     (alpha 0.001)
     (batch-size 8))
    (naked-gradient-descent
      (sampling-obj
        (l2-loss iris-classifier)
        iris-train-xs
        iris-train-ys)
      (init-theta iris-theta-shapes))))

;; model - note the inversion now, theta over t
(define model
  (lambda (classifier theta)
    (lambda (t)
      ((classifier t) theta))))

;; iris-model
(define iris-model
  (model iris-classifier iris-theta))

;; test:
(iris-model iris-test-xs)

;; next-a
(define next-a
  (lambda (t i a)
    (if (> (tref t i) (tref t a))
      i
      a)))

(next-a (tensor 3 4) 0 1)
(next-a (tensor 4 3) 0 1)

;; argmaxed
(define argmaxed
  (lambda (t i a)
    (let ((a (next-a t i a))) ;; make sure we get a chance to run next-a for idx 0
      (if (zero? i)
        a
        (argmaxed t (sub1 i) a)))))

;; argmax-1
(define argmax-1
  (lambda (t)
    (let ((i (sub1 (tlen t))))
      (argmaxed t i i))))

(argmax-1 (tensor 3 8 99 -2 14))
(argmax-1 (tensor 3 8 99 -2 101))
(argmax-1 (tensor 101 8 99 -2 14))

;; class=1
(define class=1
  (lambda (t1 t2)
    (if (= (argmax-1 t1) (argmax-1 t2))
      1.0
      0.0)))

(class=1 (tensor 2 1 18) (tensor -3 -77 1))

;; FLUB:
(define class= (ext2 class=1 1 1)) ;; 1 1 => ?

;; accurate-enough-iris-theta?
;; - given a theta & a classifier
;; - build a model
;; - apply it to training xs
;; - so now we have pred-ys and ys
;; - map -> sum -> / length of xs -> > 90?
(define accurate-enough-iris-theta?
  (lambda (theta)
    (let ((a-model (model iris-classifier theta)))
      (>
        (/
          (sum
            (class= (a-model iris-test-xs) iris-test-ys))
          (tlen iris-test-xs))
        0.9))))

;; [grid-search]
(grid-search
  accurate-enough-iris-theta?
  ((revs 500 1000 2000 4000)
   (alpha 0.001 0.002 0.004)
   (batch-size 4 8 16))
  (naked-gradient-descent
    (sampling-obj
      (l2-loss iris-classifier)
      iris-train-xs
      iris-train-ys)
    (init-theta iris-theta-shapes)))
