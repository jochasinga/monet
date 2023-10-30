(define-module (lexer keyword)
  #:use-module (lexer pred)
  #:export (get-keyword
            keyword?))

(define (inner-keyword port acc)
  (let ((c (peek-char port)))
    (cond
     ((eof-object? c) (keyword* port acc))
     ((is-delimiter? c) (return-keyword acc))
     (else (inner-keyword port (cons (read-char port) acc))))))

(define (keyword? s) 
  (> (string-length 
      (get-keyword (open-input-string 
                    (cond 
                     ((string? s) s)
                     ((symbol? s) (symbol->string s)))))) 
     0))

(define keywords 
  (list "Close" "When" "Case" 
        "Deposit" "Pay" "DealProposalCreated" 
        "DealPublished" "DealTerminated"
        "Role" "Token" "Address" "MulValue" "Constant" 
        "ConstantParam" "Party" "TimeParam"))

(define (return-keyword acc)
  (if (null? acc)
      ""
      (let ((keyword (string-join (reverse (map string acc)) "")))
        (cond
         ((member keyword keywords) keyword)
         (else "")))))

(define (is-capitalized-c-or-w? c) 
  (or (char=? #\C c)
      (char=? #\W c)))

(define (keyword* port acc)
  (let ((c (peek-char port)))
    (cond
     ((eof-object? c) (return-keyword acc))
     ((char-upper-case? c)
      (read-char port)
      (inner-keyword port (cons c acc)))
     (else
      (read-char port)
      (keyword* port acc)))))

(define (get-keyword port) (keyword* port '()))

