#lang racket/base
(require racket/contract
         racket/match
         racket/list
         "runtime.rkt"
         "ast.rkt")

(define remove-paths
  (match-lambda
    [(? hash? ht)
     (for/hash ([(k v) (in-hash ht)])
       (values k (remove-paths v)))]
    [(? cons? c)
     (cons (remove-paths (car c))
           (remove-paths (cdr c)))]
    [(? prefab-struct-key s)
     (apply make-prefab-struct
            (prefab-struct-key s)
            (remove-paths (rest (vector->list (struct->vector s)))))]
    [(? path? s)
     #f]
    [x x]))

(define (write-theory t [out (current-output-port)])
  (write (remove-paths t) out))

(define (read-theory [in (current-input-port)])
  (hash-copy (read in)))

(provide/contract
 [write-theory (->* (theory/c) (output-port?) void?)]
 [read-theory (->* () (input-port?) theory/c)])
