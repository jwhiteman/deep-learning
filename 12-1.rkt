(require malt)
;; network: (64 32) (64) (45 64) (45) (26 45) (26)

;; justification: composition of layer functions with their shape lists

(define block
  (lambda (block-fn shape-list)
    (list block-fn shape-list)))

(define block-fn
  (lambda (block)
    (car block)))

(define block-ls
  (lambda (block)
    (cadr block)))

(define dense-block
  (lambda (block-fn n m)
    (block block-fn
           (list
             (list n m)
             (list n)))))

(define layer1 (dense-block relu 64 32))
(define layer2 (dense-block relu 45 64))
(define layer3 (dense-block relu 26 45))

;; block-compose?
;; why t => theta? because while layer functions are target functions, their
;; composition must also be a target function.
(define block-compose
  (lambda (f g j)
    (lambda (t)
      (lambda (theta)
        (f
          ((g t) theta)
          (drop theta j))))))

;; stack2
(define stack2
  (lambda (ba bb)
    (block
      (block-compose
        (block-fn ba)
        (block-fn bb)
        (len (block-ls ba))) ;; how much of theta belongs to ba; amount to be cleared out for bb
      (append
        (block-ls ba)
        (block-ls bb)))))

(stack2 layer1 layer2)

;; stack-blocks:
(define stacked-blocks
  (lambda (rbls block)
    (if (null? rbls)
      block
      (stacked-blocks (cdr rbls)
                      (stack2 block
                              (car rbls))))))
(define stack-blocks
  (lambda (bls)
    (stacked-blocks (cdr bls)
                    (car bls))))

(stack-blocks
  (list layer1 layer2 layer3))
