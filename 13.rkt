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

;; block-compose
;; stack2
;; stack-blocks
;; stacked-blocks
(define iris-network
  (stack-blocks
    (list
      (dense-block 4 6)    ;; 6 neurons - 4 wide
      (dense-block 6 3)))) ;; 3 neurons - 6 wide

;; ?
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
