(require malt)

(define block
  (lambda (fn shape-list)
    (list fn shape-list)))

(define layer1
  (block relu
         (list
           (list 64 32)  ;; 64 neurons, t is a tensor-1 32 wide
           (list 64))))  ;; 64 biases (1 per neuron)
                         ;; output: tensor-1 64 wide
(define layer2
  (block relu
         (list
           (list 45 64)
           (list 45))))  ;; output: tensor-1 45 wide

(define layer3
  (block relu
         (list
           (list 26 45)
           (list 26))))

;; use a function to build these:
(define dense-block
  (lambda (n m)
    (block relu
           (list
             (list n m)
             (list n)))))

(define layer1 (dense-block 64 32))
(define layer2 (dense-block 45 64))
(define layer3 (dense-block 26 45))

(define block-fn
  (lambda (ba)
    (list-ref ba 0)))

(define block-ls
  (lambda (ba)
    (list-ref ba 1)))


;; GOAL: implement k-relu but more flexible so that relu isn't fixed as the layer function

;; f & g are layer functions (e.g relu)
;; eacher layer function needs:
;; let n be the number of neurons, m the width
;; theta θ:     nxm weights
;; theta θn+1:  n   biases
;; f & g are both arbitrary layer functions
;; θ contains all weights & biases for both f & g
;; apply f to t => θ(0, 1) => which becomes the t input for g, which is applied
;; to θ(2, 3)
;;
;; NOTE: this is a target function builder, no? some-params => t => θ
(define block-compose
  (lambda (f g j)
    (lambda (t)
      (lambda (theta)
        ((g
           ((f t) theta))
         (refer theta j))))))

(block-compose relu relu 2)

;; append:
(append '(1 2) '(3 4 5))

;; to merge to blocks (see layer1, etc)
;; note: this is just a helper that delegates, in part, to block-compose;
;; t & theta aren't used.
;; 1. given two blocks, let's compose the fn using block-compose, such that
;; the output of 1 becomes the input of the other.
;; 2. note that (tlen block-ls ba) could probably be hardcoded to two;
;; we use (list (list n m) (list n)), which is a list of two, which is a way
;; of saying: each layer needs its weights & biases. maybe future versions
;; here will use more than weights & biases. attention?
;; 3. because we're creating a new block, merge the shape lists by appending
;; them together. I guess this appending means we /couldn't/ hardcode to two:
;; each block might be several blocks stacked together. A stack2 block would
;; have a tlen of 4.
(define stack2
  (lambda (ba bb)
    (block
      (block-compose
        (block-fn ba)
        (block-fn bb)
        (len (block-ls ba)))
      (append
        (block-ls ba)
        (block-ls bb)))))

(stack2 layer1 layer2)

;; so we can use stack2 to merge/compose two blocks (two layer functions
;; w/ their appropriate shape lists)
;; now we need a way of doing this for a list of blocks.
;; we'll probably accumulate: merge two blocks, then merge the third with
;; the first two, and so on.
(define stack-blocks
  (lambda (bls) ;; block list
    (stacked-blocks (refr bls 1)         ;; refr is like drop
                    (list-ref bls 0))))

(define stacked-blocks
  (lambda (rbls ba)  ;; rbls - rest block list
    (if (null? rbls)
      ba
      (stacked-blocks (refr rbls 1)
                      (stack2 ba (list-ref rbls 0))))))

(stack-blocks (list layer1 layer2 layer3))

;; QUESTIONS: how is the shape-list actually used? validation?
