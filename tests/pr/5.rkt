#lang racket
(require datalog)
(datalog! (make-theory) (! (:- (p ||))))
