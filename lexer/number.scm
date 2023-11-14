(define-module (lexer number)
  #:use-module (lexer pred)
  #:use-module (lexer fixnum)
  #:use-module (lexer decimal)
  #:use-module (ice-9 match)
  #:export (get-number-type
            unread-all-chars
            number*
            fixnum?
            decimal?))

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

(define (fixnum? e)
  (let* ((ex (cond
             ((number? e) (number->string e))
             ((symbol? e) (symbol->string e))))
         (p (call-with-input-string ex (lambda (p) (number* p)))))    
    (match p
      ((_ 'fixnum) #t)
      (_ #f))))

(define (decimal? e)
  (let* ((ex (cond
              ((number? e) (number->string e))
              ((symbol? e) (symbol->string e))))
         (p (call-with-input-string ex (lambda (p) (number* p)))))    
    (match p
      ((_ 'decimal) #t)
      (_ #f))))

(define (number* port)
  (let lp ((c (peek-char port))
           (type 'nan)
           (acc '()))
    (cond
     ((or (eof-object? c) (is-delimiter? c)) 
      (match (reverse acc)
       ((#\0 #\x (? is-number?) ..1) 
        (list (unread-all-chars port acc) 'fixnum))
       (( (? is-digit?) ..1 ) (list (unread-all-chars port acc) 'fixnum))
       (( (or (? is-dash?) (? is-plus?)) (? is-digit?) ..1 ) (list (unread-all-chars port acc) 'fixnum))
       (_ (list (unread-all-chars port acc) type))))
     ((is-point? c)
      (list (unread-all-chars port acc) 'decimal))
     ((is-underscore? c)
      (read-char port)
      (lp (peek-char port) type acc))
     ((or (is-dash? c) (is-plus? c))
      (read-char port)
      (lp (peek-char port) type (cons c acc)))
     ((and (not (is-number? c)) (not (is-x? c)))  
      (list (unread-all-chars port acc) 'nan))
     (else 
      (read-char port)
      (lp (peek-char port) type (cons c acc))))))

