(define-module (lexer bool)
  #:use-module (lexer pred)
  #:use-module (ice-9 match)
  #:export (get-bool bool? bool->boolean))

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

(define (bool? s)
  (cond
   ((symbol? s) (or (eq? s ':f) (eq? s ':t)))
   ((string? s) (or (string=? s ":f") (string=? s ":t")))
   (else #f)))

(define (bool->boolean e)
  (cond
   ((symbol? e) (match e
                  (':f #f)
                  (':t #t)
                  (_ (error "not a bool"))))
   ((string? e) (match e
                  (":f" #f)
                  (":t" #t)
                  (_ (error "not a bool"))))))


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


