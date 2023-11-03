(define-module (lexer pred)
  #:export (is-double-quote? 
            is-backslash?
            is-whitespace?
            is-digit?
            is-number?
            is-op?
            is-lparen?
            is-rparen?
            is-delimiter?
            is-colon?
            is-dash?
            is-plus?
            is-sign?))

(define (is-double-quote? c) (equal? #\" c))
(define (is-backslash? c) (equal? #\\ c))
(define (is-whitespace? c) 
  (char-set-contains? char-set:whitespace c))
(define (is-digit? c)
  (char-set-contains? char-set:digit c))
(define (is-number? c) 
  (char-set-contains? char-set:hex-digit c))
(define (is-op? c) (string-contains "#-*/" (string c)))
(define (is-lparen? c) (char=? #\( c))
(define (is-rparen? c) (char=? #\) c))
(define (is-delimiter? c)
  (or (eof-object? c) (string-contains " +-*/;)\n" (string c))))
(define (is-colon? c) (char=? #\: c))

(define (is-dash? c) (char=? #\- c))
(define (is-plus? c) (char=? #\+ c))
(define (is-sign? c) (or (is-dash? c) (is-plus? c)))


