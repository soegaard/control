#lang racket
(provide begin/goto goto label)
(require racket/stxparam)
(require (for-syntax syntax/parse racket/list syntax/stx racket/stxparam))

;;;
;;; BEGIN WITH GOTO AND LABELS
;;;

; This file implements a simple begin with gotos.
; For a more general version use tagged-begin.
; See the bottom of this file for examples.


; Make sure using goto outside a begin/goto leads to a sensible error message
(define-syntax-parameter goto
  (λ (stx) (raise-syntax-error 'goto "identifier used outside begin/goto"  stx)))
(define-syntax-parameter label
  (λ (stx) (raise-syntax-error 'label "identifier used outside begin/goto" stx)))

;;;
;;; EXAMPLES
;;;

;;; The syntax transformer of begin/goto must break up the body
;;; into labelled blocks. Each block becomes a bound in a letrec expressions,
;;; in which the labels are bound to its block.

;;; In order to continue from one block to the next, the last expression
;;; in block must be a goto to the next block. Except if the last expression
;;; in block is a goto - in which case no new goto expression must be inserted.

#;(begin/goto
    (label foo)
    1
    (label bar)
    (goto foo))
; =>
#;(letrec ([foo (lambda () 1 (goto bar))]      ; continue to bar
           [bar (lambda ()   (goto foo))])     ; no new goto inserted
    (foo))

(define-for-syntax (label? stx)
  (syntax-parse stx #:literals (label)
    [(label label-name) #t]
    [_else              #f]))

(define-for-syntax (goto? stx)
  (syntax-parse stx #:literals (goto)
    [(goto label-name) #t]
    [_else             #f]))

(define-for-syntax (non-label? stx)
  (not (label? stx)))

; (labels-and-exprs->blocks label-or-expr ...)
;  Given a syntax object with a list of label and expressions in some order
;  Returns a list of syntax objects where the elements are of the form (label expr ...)
;  In other words, this function splits the body of begin/goto in the sections
;  which are to become blocks.
(define-for-syntax (labels-and-exprs->blocks stx)
  (syntax-parse stx #:literals (label)
    [()     '()]
    [_else  (let-values ([(first more) (first-label-and-block+more stx)])
              (cons first (labels-and-exprs->blocks more)))]))

; (first-label-and-block+more label-or-expr ...)
; This is a helper function for labels-and-exprs->blocks
; given:                 a list of labels and expressions, the first of which is a label
; returns two values: 1. syntax object with the first label and the following expressions
;                     2. syntax object with what comes after the last expression
(define-for-syntax (first-label-and-block+more stx)
  (syntax-parse stx #:literals (label)
    [((label label-name) label-or-expr ...)
     (with-syntax ([(expr ...) (let ([exprs (takef (syntax->list #'(label-or-expr ...)) non-label?)])
                                 (if (null? exprs) (list #'(void)) exprs))]
                   [more       (dropf (syntax->list #'(label-or-expr ...))              non-label?)])
       (values #'(label-name (expr ...))
               #'more))]))


; name-of-label : stx -> identifier
;   Given (label name) returns name
(define-for-syntax (name-of-label stx)
  (syntax-parse stx #:literals (label)
    [(label name) #'name]))


(define-for-syntax (error-check-begin/goto stx)
  (syntax-parse stx 
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
  (syntax-parse stx #:literals (goto label)
    ; In the case goto, label, more ... we already have a label after the goto,
    ; so we just need to rewrite more.
    [((goto label-name) (label label-name1) label-or-expr ...)
     (with-syntax ([((goto label-name)    (label label-name1) label-or-expr ...) stx])
       (with-syntax ([(label-or-expr ...) (introduce-labels-after-goto #'(label-or-expr ...))])
         (syntax/loc stx
           ((goto label-name) (label label-name1) label-or-expr ...))))]
    ; In the case got, expr, more ... we need to introduce a label after goto,
    ; since the breaking-into-blocks functions uses the labels to find the beginning of the blocks.
    [((goto label-name) expr label-or-expr ...)
     (with-syntax ([((goto label-name) expr label-or-expr ...) stx])
       (with-syntax ([(lab) (generate-temporaries (list #'lab))])
         (introduce-labels-after-goto 
          (syntax/loc stx
            ((goto label-name) (label lab) expr label-or-expr ...)))))]
    ; no goto at the beginning => keep it and transform the rest
    [(label-or-expr1 label-or-expr ...)
     (with-syntax ([(label-or-expr ...) (introduce-labels-after-goto #'(label-or-expr ...))])
       (syntax/loc stx
         (label-or-expr1 label-or-expr ...)))]
    [_else
     stx]))

(define-syntax (begin/goto stx)
  (error-check-begin/goto stx)
  (syntax-parse stx #:literals (label)
    [(_)
     #'(void)]
    [(_ (label start) label-or-expr ...)
     ; 1. Introduce labels after each use of goto.
     (with-syntax ([(label-or-expr ...)  (introduce-labels-after-goto #'(label-or-expr ...))])
       ; 2. Break the body into pieces. Labels mark the beginning of a new block.
       (with-syntax ([((label-name (expr ... last-expr)) ... (end-label-name (end-expr ...)))
                      (labels-and-exprs->blocks #'((label start) label-or-expr ...))])
         ; 3. While processing a block we need to know the label of the following block
         ;    (so we can insert a "continue" in the end of the block)
         (with-syntax ([(next-label ...) (cdr (syntax->list #'(label-name ... end-label-name)))])
           ; 4. Unless the last-expr in a block is a goto, we need to insert a call
           ;    to the next block as the last expression in the block.
           (with-syntax ([(continue ...)
                          (map (lambda (last-expr next-label)
                                 (syntax-parse last-expr #:literals (goto)
                                   [(goto name) last-expr]
                                   [_else       #`(begin #,last-expr (#,next-label))]))
                               (syntax->list #'(last-expr ...))
                               (syntax->list #'(next-label ...)))])
             ; 5. Get a list of all label names used (allows error checking later)
             (with-syntax ([(name1 ...) 
                            (map name-of-label 
                                 (filter label? (syntax->list #'((label start) label-or-expr ...))))])
               ; 6. Finally ready to produce the letrec expression
               (syntax/loc stx
                 (letrec-syntaxes+values
                     ; We need to transform (goto label) into (label).
                     ; We need to check that label is defined as a label first.                     
                     ([(Goto) ; SYNTAX  (goto label)  transfer control to block named label
                       (λ (stx)
                         (syntax-parse stx #:literals (goto)
                           [(_ name:id)
                            (unless (memf (λ(x) (module-or-top-identifier=? x #'name))
                                          (syntax->list #'(name1 ...)))
                              (raise-syntax-error 'goto "unknown label" stx))
                            (syntax/loc stx
                              (name))]
                           [_else
                            (raise-syntax-error 'goto "expected (goto <label>), got" stx)]))])
                   ; Each block becomes a thunk that automatically continues to the next block.
                   ; Any (goto label) in the body is changed to the new meaning of goto
                   ; namely (Goto label) using the syntax-parameterize.
                   ([(label-name)   (syntax-parameterize ([goto (make-rename-transformer #'Goto)])
                                      (lambda () expr ... continue))]
                    ...
                    ; In the last expressions the meaning of goto also needs to be changed.
                    [(end-label-name)  (syntax-parameterize ([goto (make-rename-transformer #'Goto)])
                                         (lambda () end-expr ...))])
                   (start))))))))]
    [(_ expr label-or-expr ...)
     (syntax/loc stx
       (begin/goto (label start) expr label-or-expr ...))]))



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
