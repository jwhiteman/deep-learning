#lang racket
(require malt)
(require malt/examples/iris)

(define dense-block
  (lambda (n m)
    (block relu
           (list
             (list m n )
             (list m)))))

(dense-block 3 4)

(define iris-network
  (stack-blocks
    (list
      (dense-block 4 6)    ;; 6 neurons - 4 wide
      (dense-block 6 3)))) ;; 3 neurons - 6 wide

(define (init-shape s)
  (cond
    ((= (len s) 1) (zero-tensor s))
    ((= (len s) 2)
     (random-tensor 0.0 (/ 2 (list-ref s 1)) s))))

(define init-theta
  (lambda (shapes)
    (map init-shape shapes)))

(define iris-classifier
  (block-fn iris-network))

(define iris-theta-shapes
  (block-ls iris-network))

(define iris-theta
  (with-hypers
    ((revs 2000)
     (alpha 0.0002)
     (batch-size 8))
    (naked-gradient-descent
      (sampling-obj (l2-loss iris-classifier)
                    iris-train-xs
                    iris-train-ys)
      (init-theta iris-theta-shapes))))

(define model
  (lambda (target theta)
    (lambda (t)
      ((target t) theta))))

(define iris-model
  (model iris-classifier iris-theta))

;; INTERLUDE VI START:
(iris-model iris-test-xs)
;; accuracy
;;   class=
;;   class=-1
;;   argmax-1
;;   argmaxed
;;   next-a

(define (next-a t i a)
  (if (> (tref t i) (tref t a))
    i
    a))

(next-a (tensor 3 4) 0 1)
(next-a (tensor 4 3) 0 1)

(define argmaxed
  (lambda  (t i a)
    (let ((a (next-a t i a)))
      (if (zero? i)
        a
        (argmaxed t (sub1 i) a)))))

(define (argmax-1 t)
  (let ((i (sub1 (tlen t))))
    (argmaxed t i i)))

(define (class=1 t u)
  (if (= (argmax-1 t) (argmax-1 u))
    1.0
    0))

(class=1 (tensor 2 1 9) (tensor 2 0 2)) ;; this returns 1.0
(class=1 (tensor 2 1 9) (tensor 3 0 2)) ;; this returns 0

(define class= (ext2 class=1 1 1))

(class= (tensor 2 1 9) (tensor 2 0 2)) ;; this returns 1.0
(class= (tensor 2 1 9) (tensor 3 0 2)) ;; this returns 0

(define accuracy
  (lambda (a-model xs ys)
    (/ (sum (class= (a-model xs) ys))
            (tlen xs))))

;; (accuracy iris-model iris-test-xs iris-test-ys)
(define accurate-enough-iris-theta?
  (lambda (theta)
    (>= (accuracy
          (model iris-classifier theta)
          iris-test-xs
          iris-test-ys)
        0.9)))

(grid-search
  accurate-enough-iris-theta?
  ((revs 500 1000 2000 4000)
   (alpha 0.0001 0.0002 0.0005)
   (batch-size 4 8 16))
  (naked-gradient-descent
    (sampling-obj
      (l2-loss iris-classifier)
      iris-train-xs
      iris-train-ys)
    (init-theta iris-theta-shapes)))

;; [grid-search]
