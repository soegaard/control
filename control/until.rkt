#lang racket
(provide until)


(require (for-syntax syntax/parse))

(define-syntax (until stx)
  (syntax-parse stx
    [(_until expr body ...)
     #'(let loop ()
         body ...
         (when (not expr)
           (loop)))]))
