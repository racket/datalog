#lang racket/base
(require datalog)

(define global-variable-thy (make-theory))

;; No error if the following line is commented out
(define (bar) (datalog global-variable-thy (? (bar))))

(define (foo) (datalog! global-variable-thy (! foo)))

(eval `(datalog! global-variable-thy (! foo))
      (variable-reference->namespace (#%variable-reference)))
