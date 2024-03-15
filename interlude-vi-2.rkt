#lang racket
(require malt)
(require malt/examples/iris)

(define dense-block
  (lambda (n m)
    (block relu
           (list
             (list n m)
             (list n)))))

(define iris-network
  (stack-blocks
    (list
      (dense-block 6 4)
      (dense-block 3 6))))

(define iris-classifier
  (block-fn iris-network))

(define iris-ls
  (block-ls iris-network))

(define init-theta
  (lambda (s)
    (cond
      ((= (len s) 1) (zero-tensor s))
      ((= (len s) 2)
       (random-tensor 0
                      (/ 2 (list-ref s 1))
                      s)))))
(define init-shape
  (lambda (ls)
    (map init-theta ls)))

(define iris-theta
  (with-hypers
    ((revs 4000)
     (alpha 0.001)
     (batch-size 8))
    (naked-gradient-descent
      (sampling-obj
        (l2-loss iris-classifier)
        iris-train-xs
        iris-train-ys)
      (init-shape iris-ls))))

(lambda model
  (lambda (classifier theta)
    (lambda (t)
      ((classifier t) theta))))

(define iris-model (model iris-classifier iris-theta))
(iris-model iris-test-xs)

(define next-a
  (lambda (t i a)
    (if (> (tref t i) (tref t a))
      i
      a)))

(define argmaxed
  (lambda (t i a)
    (let ((a (next-a t i a)))
      (if (zero? i)
        a
        (argmaxed t
                  (sub1 i)
                  a)))))

(define argmax-1
  (lambda (t)
    (let ((i (sub1 (tlen t))))
      (argmaxed t i i))))

(define class=1
  (lambda (t u)
    (if (= (argmax-1 t) (argmax-1 u))
      1.0
      0.0)))

(define class= (ext2 class=1 1 1))

(define accuracy
  (lambda (a-model xs ys)
    (/ (sum (class= (a-model xs) ys))
       (tlen xs))))

(define accurate-enough-iris-theta?
  (lambda (theta)
      (>=
        (accuracy
          (model iris-classifier theta)
          iris-test-xs
          iris-test-ys)
        0.90)))

(grid-search
  accurate-enough-iris-theta?
  ((revs 1000 2000 5000 8000)
   (alpha 0.001 0.002 0.005)
   (batch-size 4 8 16))
  (naked-gradient-descent
    (sampling-obj
      (l2-loss iris-classifier)
      iris-train-xs
      iris-train-ys)
    (init-shape iris-ls)))
