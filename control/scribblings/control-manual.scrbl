#lang scribble/doc

@(require scribble/manual
          scribble/eval
          control
          (for-label racket control))

@defmodule[control]

@title{Control}
@section{History}
@index{control}
@index{CL}
@index{tagbody}
@index{knuth}
@index{go}
@index{return}
@index{loop}
@index{iteration}
@index{goto}
@index{label}

Version 2.0

- added documentation in Scribble form

Version 1.1

- added @scheme[begin/goto]

Version 1.0

- initial version, available control structures:
  @scheme[while], @scheme[until], @scheme[dotimes], and @scheme[tagged-begin]

       
@section{Control Structures}

@defform[(while test body)]{
                              
Syntax:    @scheme[_test] should be an expression and @scheme[_body] a
           sequence of one or more expressions.

Semantics: @scheme[while] is an iteration construct. Each iteration begins by 
           evaluating the @scheme[_test] expression. If it evaluates to a 
           true value, the @scheme[_body] expressions are evaluated 
           sequentially from left to right, then the next iteration 
           begins. If the @scheme[_test] expression evaluates to false then
           iteration stops and an unspecified value is returned
           as the result of the while-expression.

Example:   @interaction[
             (require control)
             (let ([n 3] [sum 0])
               (while (> n 0)
                      (set! sum (+ sum n))
                      (set! n (- n 1)))
               sum)]
}

@defform[(until test body)]{

Syntax:    @scheme[_test] should be an expression and @scheme[_body] a
           sequence of one or more expressions.

Semantics: @scheme[until] is an iteration construct. Each iteration
           begins by evaluating the @scheme[_body] expressions 
           sequentially from left to right. The @scheme[_test] 
           expression is then evaluated. If the result is
           a true value, then the next iteration begins.
           Otherwise the iteration stops and unspecified
           value is returned as the result of the 
           until-expression.

Example:   @interaction[
           (require control)
           (let ([n 3] [sum 0])
             (until (= n 1)
                    (set! sum (+ sum n))
                    (set! n (- n 1)))
             sum)]
}

@defform[(dotimes (variable expression [finally]) body)]{

Syntax:    @scheme[_variable] should be an identifier, @scheme[_expression]
           and @scheme[_finally] (if present) should be expressions and
           @scheme[_body] a sequence of one or more expressions.

Semantics: @scheme[dotimes] is an iteration contructs. Evalutations begins 
           by evaluating @scheme[_expression]. If the result is not an 
           integer an error is signaled. If the result is zero or 
           negative, the @scheme[_body] expressions are not evaluated. 
           Otherwise the @scheme[_body] expressions are evaluated for each 
           integer from 0 up to but not including the result of 
           @scheme[_expression]. 
           
           During each evaluation of the @scheme[_body] expressions,
           @scheme[_variable] is bound to each integer.
          
           When the iteration stops @scheme[_finally] is evaluated if
           present and the result returned, otherwise @schemeresult[void] is
           returned. During evaluation of @scheme[_finally]
           the @scheme[_variable] is bound to the number of times the 
           body were evaluated.

Examples:  @interaction[
           (require control)
           
           (let ((xs '()))
             (dotimes (x 5)
                      (set! xs (cons x xs)))
             xs)
           
           (let ((xs '()))
             (dotimes (x 5 (list xs x))
                      (set! xs (cons x xs))))]
}

@defform[(tagged-begin (tag / expression)* )]{

Syntax:      @scheme[_tag] should be a symbol, and all @scheme[_tag]s should be different.

Motivation:  The macro @scheme[tagged-begin] is inspired by the Common Lisp
             construct @schemeid[tagbody].

Semantics:   The @scheme[tagged-begin] expression evaluates the expressions
             in a lexical environment, where @scheme[go] and @scheme[return] are
             are bound to functions of one argument, which will
             transfer control when called.

             As main rule the expressions will be evaluated sequentially 
             from left to right. When there are no more expressions to
             be evaluated @schemeresult[void] is returned.

             If an expression evaluates (go @scheme[_tag] then control is transfered 
             to the expression following the tag. The tags have lexical scope. 
             The dynamic extent of tag is indefinite. An @scheme[(go tag)] is allowed to 
             tranfer control to an outer tagged-begin. The call @scheme[(go tag)] has the 
             proper tail recursive property, even in situation where the call 
             syntactically is not in tail position.

             If @scheme[(return _expression)] is evaluted, the value of @scheme[_expression] is
             returned as the value of the entire @scheme[tagged-begin] form.

Examples:    @interaction[
             (require control)
             (let ([i 0])
               (tagged-begin
                loop (set! i (+ i 1))
                     (when (< i 41) (go loop)))
               i)

             (let ([odd-numbers '()]
                   [a 0])
               (tagged-begin
                start    (set! a 0)
                on-odd   (set! a (+ a 1))
                         (set! odd-numbers (cons a odd-numbers))
                         (cond
                           [(>= a  9)  (go end)]
                           [(even? a)  (go on-even)]
                           [else       (go on-odd)])
                on-even  (set! a (+ a 1))
                         (go on-odd)
                end)
               odd-numbers)]

References:  "Applications of Continuations" of Daniel P. Friedman.
}

@defform[(begin/goto label-or-goto-or-expression* )]{

Syntax:   @scheme[_label-or-goto-or-expression] is 
          either @scheme[(label _identifier)]
          or     @scheme[(goto _identifier)]
          or     @scheme[_expression].

Motivation: Think of @scheme[begin/goto] as a normal @scheme[begin], where
            @scheme[goto] can be used to jump to a control point
            named by a label. An @scheme[(goto _identifier)] will
            transfer control to the point named by the identifier.
            If the @scheme[goto-form] is one of the @scheme[_label-or-goto-expression],
            then a goto doesn't grow the control context. 
            
Examples:               

       @interaction[
       (require control)
       (let ([x 1])
         (let/ec return
           (begin/goto
             (label l1)
             (set! x (+ x 1))
             (when (= x 10000000)
               (return x))
             (goto l1)))) ; this is tail-recursive

       (let ([x 1])
         (let/ec return
           (begin/goto
             (label l1)
             (set! x (+ x 1))
             (when (= x 10000000)
               (return x))
             (goto l1) ; this is tail-recursive
             2 
             )))]
}

@index-section{}
