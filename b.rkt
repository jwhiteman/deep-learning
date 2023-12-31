#lang racket

(require racket/debugger)
(define (test-function x)
  (break) ; Sets a breakpoint
  (+ x 1))

(test-function 5)
