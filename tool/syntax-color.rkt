#lang racket/base
(require racket/contract/base
         syntax-color/lexer-contract
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "../private/lex.rkt")

(provide
 (contract-out
  [get-syntax-token lexer/c]))

(define (syn-val lex a b c d)
  (values lex a b (position-offset c) (position-offset d)))

(define (colorize-string my-start-pos)
  (define lxr
    (lexer
     [(eof) (syn-val "" 'error #f my-start-pos end-pos)]
     [(:~ #\" #\\ #\newline) (lxr input-port)]
     [(:: #\\ #\\) (lxr input-port)]
     [(:: #\\ #\newline) (lxr input-port)]
     [(:: #\\ #\") (lxr input-port)]
     [#\" (syn-val "" 'string #f my-start-pos end-pos)]
     [any-char (syn-val "" 'error #f my-start-pos end-pos)]))
  lxr)

(define get-syntax-token
  (lexer
   [(:+ whitespace)
    (syn-val lexeme 'whitespace #f start-pos end-pos)]
   [comment-re
    (syn-val lexeme 'comment #f start-pos end-pos)]
   [variable-re
    (syn-val lexeme 'symbol #f start-pos end-pos)]
   [identifier-re
    (syn-val lexeme 'identifier #f start-pos end-pos)]
   [(:or #\) #\() (syn-val lexeme 'parenthesis #f start-pos end-pos)]
   [(:or "!=" #\= #\? #\~ #\. #\, ":-") (syn-val lexeme 'parenthesis #f start-pos end-pos)]
   [(eof) (syn-val lexeme 'eof #f start-pos end-pos)]
   [#\" ((colorize-string start-pos) input-port)]
   [any-char (syn-val lexeme 'error #f start-pos end-pos)]
   [(special) (syn-val lexeme 'error #f start-pos end-pos)]
   [(special-comment) (syn-val lexeme 'error #f start-pos end-pos)]))

(module+ test
  (define p (open-input-string "\"\\}\n{}\"û@F\n`ª\"\"~\e\u009A\u0081Ä)| ||\"\"|\"}ô|ü\"û\u0081±\"@u|ΣZ}\"\u00955í;/@ýG\u0001λþ\u0082)Pbù\"\"\u0018Σþ\u008CÁ\"\"h*)W|!"))
  (let loop ()
    (define-values (lex sym b c d) (get-syntax-token p))
    (unless (eq? 'eof sym)
      (printf "~a(~v)\n" sym lex)
      (loop))))
