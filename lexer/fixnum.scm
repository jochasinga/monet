(define-module (lexer fixnum)
  #:use-module (lexer pred)
  #:export (get-fixnum fixnum->number))

#|
(define (inner-hex port acc)
  (let ((c (peek-char port)))
    (cond
     ((eof-object? c) (fixnum* port acc))
     ((is-delimiter? c) (return-fixnum acc))
     ((is-underscore? c)
      (read-char port)
      (inner-hex port acc))
     (else (inner-hex port (cons (read-char port) acc))))))
|#

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
     ((is-sign? c)
      (read-char port)
      (fixnum* port (cons c acc)))
     ((is-zero? c)
      (read-char port)
      (let ((c' (peek-char port)))
        (cond 
         ((is-x? c')
          (cond 
           ((and (not (null? acc)) (is-sign? (car acc)))
            (read-char port)
            (fixnum* port '()))
           (else 
            (read-char port)
            (inner-fixnum port (cons #\x (cons #\# acc))))))
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

(define (fixnum->number e)
  (let ((ex (cond
             ((string? e) e)
             ((number? e) (number->string e))
             ((symbol? e) (symbol->string e)))))
    (call-with-input-string ex (lambda (p) (get-fixnum p)))))
