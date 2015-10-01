#lang racket
(provide while)

; SYNTAX (while expr body ...)
;   1. Evaluate expr
;   2. If the result is true, then evaluate body ... and go to 1.
;   3. Return (void)

(require (for-syntax syntax/parse))
(define-syntax (while stx)
  (syntax-parse stx
    [(_while test body ...)
     #'(let loop ()
         (when test
           body ...
           (loop)))]
    [_
     (raise-syntax-error 
      #f "(while test-expr body ...) expected, got:" stx)]))
