(use-modules (system base lalr))
(use-modules (rnrs io ports))
(use-modules (rnrs bytevectors))
(use-modules (ice-9 match))
(use-modules (lexer string)
             (lexer keyword)
             (lexer pred)
             (lexer fixnum)
             (lexer decimal)
             (lexer number)
             (lexer enclosure))

(define-syntax-rule (port-source-location port)
  (make-source-location (port-filename port)
                        (port-line port)
                        (port-column port)
                        (false-if-exception (ftell port))
                        #f))

(define-syntax-rule (return port category value)
  (make-lexical-token category (port-source-location port) value))

(define (next-token port)
  (let ((c (peek-char port)))
    (cond 
     ((or (eof-object? c) (char=? c #\nl)) ; end of line, or end src
      '*eoi*) ; return '*eoi* because LALR module need this
     ((is-whitespace? c)
      (read-char port)
      (next-token port)) ; skip white space
     ((char-upper-case? c)
      (return port 'keyword (get-keyword port)))
     ((is-digit? c)
      (let* ((res (get-number-type port))
             (port' (car res))
             (type (cadr res)))
        (cond
         ((eq? type 'fixnum)
          (return port' type (get-fixnum port)))
         ((eq? type 'decimal)
          (return port' type (get-decimal port)))
         (else (error "NaN")))))
     ((is-op? c)
      (return port (get-op port) #f))
     ((is-double-quote? c)
      (return port 'string (get-string port)))
     ((is-lparen? c)
      (return port 'lparen (get-lparen port)))
     ((is-rparen? c)
      (return port 'rparen (get-rparen port)))
     ((is-lsqbracket? c)
      (return port 'lsqbracket (get-lsqbracket port)))
     ((is-rsqbracket? c)
      (return port 'rsqbracket (get-rsqbracket port)))
     (else
      (read-char port)
      (next-token port)))))

(define (make-simple-tokenizer port) (lambda () (next-token port)))


(define (parse port)
 (let ((input-port port))
  (let loop ((sexps '()))
    (let ((sexp (read input-port)))
      (if (eof-object? sexp)
          (reverse sexps)
          (loop (cons sexp sexps)))))))

(define (parse* port)
  (let ((input-port port))
    (let loop ((sexps '()))
      (let ((sexp (read input-port)))
        (cond 
         ((eof-object? sexp)
          (let ((exp (car (reverse sexps))))
            (match exp 
              (((? symbol?) (or (? string?) (? number?) (? symbol?)) ..1)
               (let ((op (car exp)) (args (cdr exp)))
                 (cons op (map (lambda (x) 
                                 (cond 
                                  ((string? x) (list 'string x))
                                  ((number? x)
                                   (let* ((p (open-input-string (number->string x)))
                                          (t (get-number-type p)))
                                     (list (cadr t) x)))
                                  (else ;; symbol
                                   (let* ((p (open-input-string (symbol->string x)))
                                          (tt (get-number-type p))
                                          (p' (car tt))
                                          (t (cadr tt)))
                                     (list t (cond
                                              ((eq? t 'fixnum) (get-fixnum p'))
                                              ((eq? t 'decimal) (get-decimal p'))))))))
                               args)))))))
         (else (loop (cons sexp sexps))))))))

(define (comp src e)
  (match src
    (('Close) "close")
    (('When d ... 'Close) d)))

(define (parse-init port) (parse-param* port))

(define (parse-param* port)
  (let* ((token (next-token port))
         (category (lexical-token-category token)))
    ;; Match the opening paren
    (match category
      ('lparen
       (let ((keyword (parse-keyword port)))
         (let lp ((node (list (string->symbol keyword))))
           (let* ((token' (next-token port))
                  (category' (lexical-token-category token'))
                  (value' (lexical-token-value token')))
             ;; Match the closing paren
             (match (list category' value') 
               (('rparen _) (reverse node))
               (((or 'string 'fixnum 'decimal) v) 
                (lp (cons `(,category' ,v) node)))
               (_ error "Param not terminated")))))))))

(define (parse-param port)
  (let* ((token (next-token port))
         (category (lexical-token-category token))
         (value (lexical-token-value token))
         (pair (list category value)))
    (match pair
      (('lparen _)
       (let* ((keyword (parse-keyword port))
              (val (parse-value port)))
         (let* ((token''' (next-token port))
                (category''' (lexical-token-category token''')))
           (if (eq? category''' 'rparen)
               (list (string->symbol keyword) val)
               (error "Param not terminated!"))))))))
            
(define (parse-keyword port)
  (let* ((tok (next-token port))
         (category (lexical-token-category tok))
         (value (lexical-token-value tok))
         (pair (list category value)))
    (match pair
      (('keyword v) v)
      (_ (error "Not a keyword")))))

(define (parse-value port)
  (define (parse port)
    (let* ((token (next-token port))
           (category (lexical-token-category token))
           (value (lexical-token-value token))
           (pair (list category value)))
      (match pair
        (((or 'fixnum 'decimal string) _) pair)
        (_ (error "Not a value")))))
  (parse port))






#|
(define my-parser
  (lalr-parser
   ; Terminal symbols
   (program (Close) : $1
            (*eoi*) : (call-with-input-string "" read))
   ))
|#   


