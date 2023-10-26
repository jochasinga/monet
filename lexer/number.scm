(define-module (lexer number)
  #:use-module (lexer pred)
  #:use-module (lexer fixnum)
  #:use-module (lexer decimal)
  #:export (get-number-type
            unread-all-chars
            number*))

(define (is-underscore? c) (char=? #\_ c))
(define (is-point? c) (char=? #\. c))
(define (is-float? x)
  (and (real? x) (not (integer? x))))

;; TODO: Very crude and ineffective way of figuring out a
;; number type.
(define (get-number-type port) (number* port))
(define (unread-all-chars port acc)
  (cond 
   ((null? acc) port)
   (else 
    (unread-char (car acc) port)
    (unread-all-chars port (cdr acc)))))

(define (number* port)
  (let lp ((c (peek-char port))
           (type 'fixnum)
           (acc '()))
    (cond
     ((eof-object? c)
      (list (unread-all-chars port acc) type))
     ((is-point? c)
      (list (unread-all-chars port acc) 'decimal))
     ((is-delimiter? c) 
      (list (unread-all-chars port acc) type))
     ((is-underscore? c)
      (read-char port)
      (lp (peek-char port) type (cons c acc)))
     ((and (not (is-number? c)) (not (is-x? c)))  
      (list (unread-all-chars port acc) 'nan))
     (else 
      (read-char port)
      (lp (peek-char port) type (cons c acc))))))

