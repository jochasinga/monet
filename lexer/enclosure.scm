 (define-module (lexer enclosure)
   #:use-module (lexer pred)
   #:export (get-lparen
             get-rparen
             get-lsqbracket
             get-rsqbracket))

(define (get-lparen port)
  (let lp ((c (peek-char port)))
    (cond
     ((is-lparen? c) (read-char port))
     (else 
      (read-char port)
      (lp (peek-char port))))))

(define (get-rparen port)
  (let lp ((c (peek-char port)))
    (cond
     ((is-rparen? c) (read-char port))
     (else 
      (read-char port)
      (lp (peek-char port))))))

(define (get-lsqbracket port)
  (let lp ((c (peek-char port)))
    (cond
     ((is-lsqbracket? c) (read-char port))
     (else 
      (read-char port)
      (lp (peek-char port))))))

(define (get-rsqbracket port)
  (let lp ((c (peek-char port)))
    (cond
     ((is-rsqbracket? c) (read-char port))
     (else 
      (read-char port)
      (lp (peek-char port))))))




