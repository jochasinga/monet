(define-module (lexer decimal)
  #:use-module (lexer pred)
  #:export (get-decimal decimal->number))

(define (inner-fractional port acc1 acc2)
  (let ((c (peek-char port)))
    (cond
     ((eof-object? c) (decimal* port acc1 acc2))
     ((is-delimiter? c) (return-decimal acc1 acc2))
     ((is-point? c) (error "More than one decimal point not allowed"))
     ((is-underscore? c)
      (read-char port)
      (inner-fractional port acc1 acc2))
     (else (inner-fractional port acc1 (cons (read-char port) acc2))))))

(define (inner-integer port acc1 acc2)  
  (let ((c (peek-char port)))
    (cond
     ((eof-object? c) (decimal* port acc1 acc2))
     ((is-delimiter? c) (return-decimal acc1 acc2))
     ((is-point? c)
      (read-char port)
      (inner-fractional port acc1 acc2))
     ((is-underscore? c)
      (read-char port)
      (inner-integer port acc1 acc2))
     (else (inner-integer port (cons (read-char port) acc1) acc2)))))

(define (return-decimal acc1 acc2)
  (string->number
   (string-append
    (list->string (reverse acc1))
    "."
    (list->string (reverse acc2)))))

(define (decimal* port acc1 acc2)
  (let ((c (peek-char port)))
    (cond
     ((eof-object? c) (return-decimal acc1 acc2))
     ((is-sign? c)
      (read-char port)
      (decimal* port (cons c acc1) acc2))
     ((is-digit? c)
      (read-char port)
      (inner-integer port (cons c acc1) acc2))
     (else 
      (read-char port)
      (decimal* port acc1 acc2)))))

(define (get-decimal port) (decimal* port '() '()))

(define (decimal->number e)
   (let ((ex (cond
              ((string? e) e)
              ((number? e) (number->string e))
              ((symbol? e) (symbol->string e)))))
     (call-with-input-string ex (lambda (p) (get-decimal p)))))
