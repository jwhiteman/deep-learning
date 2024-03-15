#lang racket
(require malt)
(require malt/examples/iris)

;; layer + shape list
(define dense-block
  (lambda (n m)
    (block relu
           (list
             (list m n)    ;; note the switch from n m to m n
             (list m)))))

(dense-block 3 4)

;; compose two layers & name
(define iris-network
  (stack-blocks
    (list
      (dense-block 4 6)    ;; 6 neurons - 4 wide
      (dense-block 6 3)))) ;; 3 neurons - 6 wide

;; init biases & weights - map function
(define (init-shape s)
  (cond
    ((= (len s) 1) (zero-tensor s))                 ;; biases
    ((= (len s) 2)                                  ;; weights
     (random-tensor 0.0 (/ 2 (list-ref s 1)) s))))  ;; 0/variance/tensor ; idx 1 is width, so 2/width

;; maps the appended shape list (stack blocks) w/ init-shape
(define init-theta
  (lambda (shapes)
    (map init-shape shapes)))

;; extract the fn from the network block
(define iris-classifier
  (block-fn iris-network))

;; extract the shapelist from the network block
(define iris-theta-shapes
  (block-ls iris-network))

;; train the network: minimized theta for the network, w/ given hyperparams
(define iris-theta
  (with-hypers
    ((revs 2000)
     (alpha 0.0002)
     (batch-size 8))
    (naked-gradient-descent
      (sampling-obj (l2-loss iris-classifier)   ;; build objective function w/ network -> loss
                    iris-train-xs
                    iris-train-ys)
      (init-theta iris-theta-shapes))))         ;; init using the shapelist

;; helper: allow a network + trained weights/biases to take a given t (test, etc)
(define model
  (lambda (target theta)
    (lambda (t)
      ((target t) theta))))

;; the iris network + trained weights/biases
(define iris-model
  (model iris-classifier iris-theta))

;; INTERLUDE VI START:
(iris-model iris-test-xs)

;; given a tensor and two indices, return the index of the max value
(define (next-a t i a)
  (if (> (tref t i) (tref t a))
    i
    a))

(next-a (tensor 3 4) 0 1)
(next-a (tensor 4 3) 0 1)

;; given a tensor, a countdown index & a max index, countdown to zero and
;; return the max index
(define argmaxed
  (lambda (t i a)
    (let ((a (next-a t i a)))
      (if (zero? i)
        a
        (argmaxed t (sub1 i) a)))))

;; front for argmax, accumulator
(define (argmax-1 t)
  (let ((i (sub1 (tlen t))))
    (argmaxed t i i)))

;; given two one-hot-like tensor-1, return 1.0 if they agree on max index, 0
;; if they don't, i.e a classification error
(define (class=1 t u)
  (if (= (argmax-1 t) (argmax-1 u))
    1.0
    0))

(class=1 (tensor 2 1 9) (tensor 2 0 2)) ;; this returns 1.0
(class=1 (tensor 2 1 9) (tensor 3 0 2)) ;; this returns 0

;; extend
(define class= (ext2 class=1 1 1))

(class= (tensor 2 1 9) (tensor 2 0 2)) ;; this returns 1.0
(class= (tensor 2 1 9) (tensor 3 0 2)) ;; this returns 0

;; do the pred-ys from testing (using the trained model)
;; agree with the expected values? sum the 1s and divide by total
(define accuracy
  (lambda (a-model xs ys)
    (/ (sum (class= (a-model xs) ys))
            (tlen xs))))

;; given some trained theta, and some test data, is it 90% accurate?
(define accurate-enough-iris-theta?
  (lambda (theta)
    (>= (accuracy
          (model iris-classifier theta)
          iris-test-xs
          iris-test-ys)
        0.9)))

;; given a list of hyperparms, return a combo of hyperparams that allow theta
;; to be accurate enough:
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

;; plug those in to find the theta (weights/biases), save it in the model
