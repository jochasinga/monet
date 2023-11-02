(define-module (lexer bool)
  #:use-module (lexer pred)
  #:export (get-bool bool?))

(define (bool? s))

(define (inner-bool port acc)
  (let ((c (peek-char port)))
    (cond
     ;((or (char=? #\t c) (char=? #\f c)) (cons (read-char port) acc))
     ((eof-object? c) (bool* port acc))
     ((is-delimiter? c) (return-bool acc))
     (else 
      (read-char port)
      (inner-bool port (cons c acc))))))

(define (return-bool acc)
  (cond
   ((char=? (car acc) #\t) #t)
   ((char=? (car acc) #\f) #f)
   (else (error "Wrong boolean type format"))))

(define (bool** port)
  (let loop ((c (peek-char port)) (acc '()))
    (cond 
     ((eof-object? c) (return-bool acc))
     ((char=? #\: c)
      (read-char port)
      (loop (peek-char port) acc))
     ((or (char=? #\t c) (char=? #\f c))
      (read-char port)
      (loop (peek-char port) (cons c acc)))
     ((is-delimiter? c) 
      (cond 
       ((null? acc)
        (read-char port)
        (loop (peek-char port) acc))
       (else (return-bool acc))))
     (else 
      (read-char port)
      (loop (peek-char port) acc)))))

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


