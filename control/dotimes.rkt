#lang racket
(provide dotimes)

(require (for-syntax syntax/parse))

; macro: (dotimes (var expr [finally]) body ...)
;   dotimes iterates over a series of integers.
;   dotimes evaluates expr and signals an error if the result
;   is not an integer. If expr is zero or negative, the
;   body is not executed. Otherwiese dotimes executed the body
;   for each integer from 0 up to but not including the value of expr.
;   During the evaluation of body, var is bound to each integer.
;   Then finally is evaluated if present, and the result is returned,
;   otherwise #void is returned. At the time finally is evaluated,
;   var is bound to the number of times body was excuted.

(define-syntax (dotimes stx)
  (syntax-parse stx
    [(_dotimes (var count-form finally) body ...)
     #`(let ([count count-form])
         (unless (integer? count)
           (raise-syntax-error 'dotimes 
                               (format "expected integer as result of expression, got ~s " count)
                               #'count-form))
         (if (positive? count)
             (let ([var 0])
               (let loop ([i 0])
                 (set! var i)
                 (if (< i count)
                     (let ()
                       body ...
                       (loop (add1 i)))
                     finally)))
             (let ([var 0])
               finally)))]
    [(_dotimes (var count-form) body ...)
     #'(dotimes (var count-form (void)) body ...)]
    [_
     (raise-syntax-error 
      #f "bad syntax, (dotimes (var expr [finally-expr]) body ...) expected" stx)]))