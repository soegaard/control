#lang racket
(provide begin/goto)

;;; begin-with-goto.scm  --  Jens Axel Søgaard  -- 7th July 2007

; This file implements a simple begin with gotos.
; For a more general version use tagged-begin.
; See the bottom of this file for examples.

#;(begin/goto
    (label foo)
    1
    (label bar)
    (goto foo))
; =>
#;(letrec ([foo (lambda ()
                  1
                  (goto bar))]
           [bar  (lambda ()
                   (goto foo))])
    (foo))

#;(require-for-syntax 
   (only (lib "1.ss" "srfi") take-while)
   (only (lib "1.ss" "srfi") drop-while)
   (only (lib "1.ss" "srfi") filter)
   (prefix srfi: (lib "1.ss" "srfi"))
   (lib "stx.ss" "syntax"))

(require (for-syntax racket/list syntax/stx))


(define-for-syntax (label? stx)
  (syntax-case stx (label)
    [(label label-name) #t]
    [_else              #f]))

(define-for-syntax (goto? stx)
  (syntax-case stx (goto)
    [(goto label-name) #t]
    [_else             #f]))

(define-for-syntax (non-label? stx)
  (not (label? stx)))

(define-for-syntax (first-label-and-block+more stx)
  (syntax-case stx (label)
    [((label label-name) label-or-expr ...)
     (with-syntax ([(expr ...)
                    (let ([exprs
                           (takef (syntax->list #'(label-or-expr ...))
                                  non-label?)])
                      (if (null? exprs) (list #'(void)) exprs))]
                   [more 
                    (dropf (syntax->list #'(label-or-expr ...))
                           non-label?)])
       (values #'(label-name (expr ...))
               #'more))]))

(define-for-syntax (labels-and-exprs->blocks stx)
  (syntax-case stx (label)
    [()     '()]
    [_else  (let-values ([(first more) (first-label-and-block+more stx)])
              (cons first (labels-and-exprs->blocks more)))]))

(define-for-syntax (name-of-label stx)
  (syntax-case stx (label)
    [(label name) #'name]))

(define-for-syntax (error-check-begin/goto stx)
  (syntax-case stx ()
    [(_ label-or-expr ...)
     (let* ([labels (filter label? (syntax->list #'(label-or-expr ...)))]
            [names  (map name-of-label labels)])
       ; Are all labels identifiers?
       (for-each (lambda (name)
                   (unless (identifier? name)
                     (raise-syntax-error 'begin/goto
                                         "labels must be identifiers" name)))
                 names)
       ; Are the duplicate labels?
       (cond
         [(check-duplicate-identifier names)
          => (lambda (name)
               (raise-syntax-error 'begin/goto
                                   "duplicate label found: "
                                   name))]))]))

(define-for-syntax (introduce-labels-after-goto stx)
  (syntax-case stx (goto label)
    [((goto label-name) (label label-name1) label-or-expr ...)
     (with-syntax ([((goto label-name) (label label-name1) label-or-expr ...)
                    stx])
       (with-syntax ([(label-or-expr ...)
                      (introduce-labels-after-goto
                       #'(label-or-expr ...))])
         (syntax/loc stx
           ((goto label-name) (label label-name1) label-or-expr ...))))]
    [((goto label-name) expr label-or-expr ...)
     (with-syntax ([((goto label-name) expr label-or-expr ...) stx])
       (with-syntax ([(lab) (generate-temporaries (list #'lab))])
         (introduce-labels-after-goto 
          (syntax/loc stx
            ((goto label-name) (label lab) expr label-or-expr ...)))))]
    [(label-or-expr1 label-or-expr ...)
     (with-syntax ([(label-or-expr ...)
                    (introduce-labels-after-goto
                     #'(label-or-expr ...))])
       (syntax/loc stx
         (label-or-expr1 label-or-expr ...)))]
    [_else
     stx]))


(define-syntax (begin/goto stx)
  (error-check-begin/goto stx)
  (syntax-case stx (label)
    [(_) 
     #'(void)]
    [(_ (label start) label-or-expr ...)
     (with-syntax ([(label-or-expr ...) 
                    (introduce-labels-after-goto #'(label-or-expr ...))])
       (with-syntax ([((label-name (expr ... last-expr)) ... (end-label-name (end-expr ...)))
                      (labels-and-exprs->blocks #'((label start) label-or-expr ...))])
         (with-syntax ([(next-label ...)
                        (cdr (syntax->list #'(label-name ... end-label-name)))])
           (with-syntax ([(continue ...)
                          (map (lambda (last-expr next-label)
                                 (syntax-case last-expr (goto)
                                   [(goto name) last-expr]
                                   [_else       #`(begin #,last-expr (#,next-label))]))
                               (syntax->list #'(last-expr ...))
                               (syntax->list #'(next-label ...)))])
             (with-syntax ([(name1 ...) 
                            (map name-of-label 
                                 (filter label? (syntax->list #'((label start) label-or-expr ...))))])
               (with-syntax ([goto (syntax-local-introduce #'goto)])
                 (syntax/loc stx
                   (letrec-syntaxes+values
                       ([(goto) (lambda (stx)
                                  (syntax-case stx (goto)
                                    [(_ name)
                                     (begin
                                       (unless (identifier? #'name)
                                         (raise-syntax-error 'goto "identifier expected" #'name))
                                       (cond
                                         [(memf (λ(x) 
                                                  ; was module-identifier=?
                                                  (module-or-top-identifier=? x #'name))
                                                (syntax->list #'(name1 ...)))
                                          (syntax/loc stx
                                            (name))]
                                         [else
                                          (raise-syntax-error 'goto "unknown label" stx)]))]
                                    [_else
                                     (raise-syntax-error 'goto "expected (goto <label>), got" stx)]))])
                     ([(label-name)      (lambda () expr ... continue)]
                      ...
                      [(end-label-name)  (lambda () end-expr ...)])
                     (start)))))))))]
    [(_ expr label-or-expr ...)
     (syntax/loc stx
       (begin/goto (label start) expr label-or-expr ...))]))


;(require begin-with-goto)
;(require (planet "78.ss" ("soegaard" "srfi.plt")))
;

(module+ test (require rackunit)
  
  (check-equal? (begin/goto)  (void))
  (check-equal? (begin/goto 1)  1)
  (check-equal? (begin/goto 1 2)  2)
  (check-equal? (begin/goto 1 2 3)  3)
  
  (check-equal? (begin/goto (label l1))  (void))
  (check-equal? (begin/goto (label l1) 1)  1)
  (check-equal? (begin/goto (label l1) 1 2)  2)
  
  (check-equal? (begin/goto (label l1) (label l2))  (void))
  (check-equal? (begin/goto (label l1) (label l2) 1)  1)
  (check-equal? (begin/goto (label l1) (label l2) 1 2)  2)
  
  (check-equal? (begin/goto (goto l1) (label l1))  (void))
  (check-equal? (begin/goto (goto l1) (label l1) 1)  1)
  (check-equal? (begin/goto (goto l1) (label l1) 1 2)  2)
  
  (check-equal? (begin/goto (goto l1) 3 (label l1))  (void))
  (check-equal? (begin/goto (goto l1) 3 (label l1) 1)  1)
  (check-equal? (begin/goto (goto l1) 3 (label l1) 1 2)  2)
  
  (check-equal? (begin/goto (goto l1) (label l2) (label l1))  (void))
  (check-equal? (begin/goto (goto l1) (label l2) (label l1) 1)  1)
  (check-equal? (begin/goto (goto l1) (label l2) (label l1) 1 2)  2)
  
  (check-equal? (begin/goto (label l2) (label l1))  (void))
  (check-equal? (begin/goto (label l2) (label l1) 1)  1)
  (check-equal? (begin/goto (label l2) (label l1) 1 2)  2)
  
  (check-equal? (begin/goto (label l2) (goto l1) (label l1))  (void))
  (check-equal? (begin/goto (label l2) (goto l1) (label l1) 1)  1)
  (check-equal? (begin/goto (label l2) (goto l1) (label l1) 1 2)  2)
  
  (check-equal? (let ([x 1])
                  (begin/goto (label l1)
                              (set! x (+ x 1))
                              (if (= x 10000)
                                  (goto l2)  ; sadly not tail-recursive (use tagged-begin instead)
                                  (goto l1))
                              (label l2)
                              x))
                10000)
  
  (check-equal? (let ([x 1])
                  (let/ec return
                    (begin/goto
                      (label l1)
                      (set! x (+ x 1))
                      (when (= x 10000000)
                        (return x))
                      (goto l1)))) ; this is tail-recursive
                10000000)
  
  (check-equal? (let ([x 1])
                  (let/ec return
                    (begin/goto
                      (label l1)
                      (set! x (+ x 1))
                      (when (= x 10000000)
                        (return x))
                      (goto l1) ; unless this is tail-recursice, the stack will blow
                      2
                      )))
                10000000)
  
  ;; The following must raise syntax errors
  ;(begin/goto (label dup) (label dup))  ; duplicate label
  ;(begin/goto (goto l1))                ; non-existing label
  )