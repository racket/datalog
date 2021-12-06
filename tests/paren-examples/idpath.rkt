#lang datalog/sexp
; path test from Chen & Warren
(! (edge a b))
(! (edge b c))
(! (edge c d))
(! (edge d a))

(! (:- (node X)
       (edge X Y)))
(! (:- (node Y)
       (edge X Y)))

(! (:- (edge X X)
       (node X)))

(! (:- (path X X)
       (node X)))
(! (:- (path X Y)
       (edge X Y)))
(! (:- (path X Y)
       (edge X Z)
       (path Z Y)))
(! (:- (path X Y)
       (path X Z)
       (edge Z Y)))

(? (node X))
(? (edge X Y))
(? (path X Y))
