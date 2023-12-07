(define-module (parser)
  #:use-module (system base lalr)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (lexer string)
  #:use-module (lexer keyword)
  #:use-module (lexer pred)
  #:use-module (lexer fixnum)
  #:use-module (lexer decimal)
  #:use-module (lexer number)
  #:use-module (lexer enclosure)
  #:use-module (lexer bool)
  #:export (term? 
            expr? 
            get-expr
            parse
            match-interspersed
            next-token))

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
     ((is-colon? c)
      (return port 'bool (get-bool port)))
     ((char-upper-case? c)
      (return port 'keyword (get-keyword port)))
     ((is-sign? c)
      (read-char port)
      (let ((c' (peek-char port)))
        (cond 
         ((is-digit? c') 
          (let* ((res (get-number-type port))
                 (port' (car res))
                 (type (cadr res)))
            (unread-char c port)
            (cond 
             ((eq? type 'fixnum) 
              (return port' type (get-fixnum port)))
             ((eq? type 'decimal)
              (return port' type (get-decimal port)))))))))

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
     #|
     ((is-op? c)
      (return port (get-op port) #f))
     |#
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

(define (match-interspersed lst)
  (cond
   ((null? lst) #f)
   (else
    (let loop ((acc lst) (flag #t))
      (cond
       ((null? acc) flag)
       ((and (= (length acc) 1) (expr? (car acc))) flag)
       (else 
        (match acc
          (((? expr? lhs) (? is-op? op) rest ...)
           (loop rest flag))
          (else #f))))))))

(define (parse port)
 (let ((input-port port))
  (let loop ((sexps '()))
    (let ((sexp (read input-port)))
      (if (eof-object? sexp)
          (reverse sexps)
          (loop (cons sexp sexps)))))))

(define (term? sexp)
  (match sexp
    (( (? term? inner) ) (term? inner))
    ((_ ...) #f)
    ((or (? bool?) (? fixnum?) (? decimal?) (? string?)) #t)
    (_ #f)))

(define (expr? sexps)
  (match sexps
    ((? term?) #t)
    ;(((or (? term?) (? expr?)) (? is-op?) (or (? term?) (? expr?))) #t)
    (((? expr?) (? is-op?) (? expr?)) #t)
    (( (? expr? inner) ) (expr? inner))
    (_ (match-interspersed sexps))))

(define (! x)
  (eval x (interaction-environment)))

;; Evaluate an arithmetic expression
(define (get-expr sexp)
  (match sexp
    (( (? expr? e) ) (get-expr e))
    ((? term? t) 
     (cond
      ((fixnum? t) (fixnum->number t))
      ((decimal? t) (decimal->number t))
      ((bool? t) (bool->boolean t))
      ((string? t) t)))
    ((? expr? e)
     (match e
       ((lhs op rhs)
        (cond
         ((and (string? lhs) (eq? op '+)) (string-append (get-expr lhs) (get-expr rhs)))
         ((and (or (fixnum? lhs) (decimal? lhs)) (is-op? op))
          ((! op) (get-expr lhs) (get-expr rhs)))
         (else (error "not a valid expression"))))))
    (_ (error "not an expression"))))

(define (param? sexps)
  (match sexps
    (((? is-keyword?) (or (? string?) (? number?) (? symbol?)) ..1) #t)
    (_ #f)))

(define (action? sexps)
  (match sexps
    (((? is-keyword?) (or (? param?) (? action?)) ..1) #t)
    (_ #f)))

(define (case? sexps)
  (match sexps
    (('When ('Case (or (? action?) (? case?)) ..1) (? param?) ...) #t)
    (_ #f)))

(define (actor? sexps)
  (match sexps
    ('Close #t)
    ((? case?) #t)
    (_ #f)))

(define (parse-expr** port) (get-expr (parse port)))
(define (parse-case** port) (get-case (parse port)))
(define (parse-action** port) (get-action (car (parse port))))
(define (parse-param** port) (get-param (car (parse port))))

(define (get-case sexp)
  (match sexp
    ((? case?)
     (match sexp
       ((_ (op args ..1) p ...)
        (append 
         (cons op (map (lambda (arg)
                         (match arg
                           ((? action?) (get-action arg))
                           ((? case?) (get-case arg))
                           (_ (error "action|case|param expression not satisfied"))))
                       args)) p)
        )))))

(define (get-action sexp)
  (match sexp
    ((? action?)
     (match sexp
       ((op args ..1)
        (cons op (map (lambda (arg)
                        (match arg
                            ((? param?) (get-param arg))
                            ((? action?) (get-action arg))
                            (_ (error "param or action expression not satisfied"))))
                      args)))))))

(define (get-param sexp)
  (match sexp 
    ((? param?)
     (let ((op (car sexp)) (args (cdr sexp)))
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
                     args)))))  
  )

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

#|
(define (parse-expr port) 
  (let* ((tok (next-token port))
         (cat (lexical-token-category tok))
         (val (lexical-token-value tok))
         (p (list cat val)))
    (match p
      (('lparen _) 
       (let* ((v (parse-expr port))
              (tok' (next-token port))
              (cat' (lexical-token-category tok')))
         (if (eq? cat' 'rparen)
             (display "Expression closed out")
             (error "Expression not terminated"))))
      (('add _) ())
      (('minus _) ())
      (('times _) ())
      (('fslash _) ())
      (('fixnum _)
       )
      (('decimal _) ())
      (('string _) ())))
|#

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


