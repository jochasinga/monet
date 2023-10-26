(define-module (lexer string)
  #:use-module (lexer pred)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:export (get-string is-double-quote? is-backslash?))

(define (is-double-quote? c) (equal? #\" c))
(define (is-backslash? c) (equal? #\\ c))

(define (esc port acc)
  (let ((c (peek-char port)))
    (cond 
     ((eof-object? c) (str* port acc))
     ((is-backslash? c)
      (read-char port)
      (let ((c' (read-char port)))
        (if (is-double-quote? c')
            (inner-str port (cons c' acc))
            (error "Character escape not allowed"))))
     (else (esc port (cons (read-char port) acc))))))

(define (inner-str port acc)
  (let ((c (peek-char port)))
    (cond
     ((eof-object? c) (str* port acc))
     ((is-backslash? c)
      (read-char port)
      (let ((c' (read-char port)))
        (if (is-double-quote? c')
            (esc port (cons c' acc))
            (error "Character escape not allowed"))))

     ((is-double-quote? c) 
      (read-char port)
      (return-str acc))
     (else (inner-str port (cons (read-char port) acc))))))

(define (return-str acc)
  (if (null? acc)
      ""
      (string-join (reverse (map string acc)) "")))

(define (str* port acc)
  (let ((c (peek-char port)))
    (cond
     ((eof-object? c) (return-str acc))
     ((is-delimiter? c) (return-str acc))
     ((is-backslash? c)
      (let ((c' (read-char port)))
        (cond 
         ((is-double-quote? c') (inner-str port (cons c' acc))) 
         (else (error "Character escape not allowed")))))
     ((is-double-quote? c)
      (read-char port)
      (inner-str port acc))
     (else
      (read-char port)
      (str* port acc)))))

(define (get-string port) (str* port '()))

