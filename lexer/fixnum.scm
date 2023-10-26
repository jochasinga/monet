(define-module (lexer fixnum)
  #:use-module (lexer pred)
  #:export (get-fixnum
            fixnum*
            is-x?
            is-underscore?))

(define (is-underscore? c) (char=? #\_ c))
(define (is-zero? c) (char=? #\0 c))
(define (is-x? c) (char=? #\x c))
(define (is-point? c) (char=? #\. c))

(define (inner-hex port acc)
  (let ((c (peek-char port)))
    (cond
     ((eof-object? c) (fixnum* port acc))
     ((is-delimiter? c) (return-fixnum acc))
     ((is-underscore? c)
      (read-char port)
      (inner-hex port acc))
     (else (inner-hex port (cons (read-char port) acc))))))

(define (inner-fixnum port acc)  
  (let ((c (peek-char port)))
    (cond
     ((eof-object? c) (fixnum* port acc))
     ((is-delimiter? c) (return-fixnum acc))
     ((is-underscore? c)
      (read-char port)
      (inner-fixnum port acc))
     (else (inner-fixnum port (cons (read-char port) acc))))))

(define (return-fixnum acc)
  (string->number (list->string (reverse acc))))

(define (fixnum* port acc)
  (let ((c (peek-char port)))
    (cond
     ((eof-object? c) (return-fixnum acc))
     ((is-zero? c)
      (read-char port)
      (let ((c' (peek-char port)))
        (cond 
         ((is-x? c')
          (read-char port)
          (inner-fixnum port (cons #\x (cons #\# acc))))
         ((is-digit? c')
          (read-char port)
          (inner-fixnum port (cons c' acc))))))
     ((is-digit? c)
      (read-char port)
      (inner-fixnum port (cons c acc)))
     (else 
      (read-char port)
      (fixnum* port acc)))))

(define (get-fixnum port) (fixnum* port '()))
