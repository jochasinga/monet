(define-module (lexer bool)
  #:use-module (lexer pred)
  #:export (get-bool))

(define (inner-bool port acc)
  (let ((c (peek-char port)))
    (cond
     ((eof-object? c) (bool* port acc))
     ((is-delimiter? c) (return-bool acc))
     ((or (char=? #\t c) (char=? #\f c)) 
      (read-char port)
      (let ((c' (peek-char port)))
        (cond
         ((or (eof-object? c') (is-delimiter? c')) 
          (return-bool (cons c acc)))
         (else bool* port acc)))) 
     (else 
      (read-char port)
      (bool* port acc)))))

(define (return-bool acc)
  (if (null? acc)
      #f
      (cond
       ((char=? (car acc) #\t) ':t)
       ((char=? (car acc) #\f) ':f)
       (else #f))))

(define (bool* port acc)
  (let ((c (peek-char port)))
    (cond
     ((eof-object? c) (return-bool acc))
     ((char=? #\: c)
      (read-char port)
      (inner-bool port acc))
     (else
      (read-char port)
      (bool* port acc)))))

(define (get-bool port) (bool* port '()))


