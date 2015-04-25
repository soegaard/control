#lang setup/infotab
(define name "Control")
(define blurb
  (list "Control Structures: while, until, begin/goto, tagbody, dotimes"))
(define scribblings '(["scribblings/control-manual.scrbl"]))
(define categories '(devtools))
(define version "3.0")
(define primary-file "main.rkt")
(define compile-omit-paths '("tests" "scribblings"))
(define release-notes (list))
;(define repositories '("4.x"))