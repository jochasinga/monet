(use-modules (srfi srfi-64)
             (lexer string)
             (ice-9 popen)
             (ice-9 match))


(test-begin "get-string")


(define tt (list
            '("\"foo\"" "foo" "test-string" #f)
            '(":t" #f "test-bool" #f)
            '("  foo bar :t \"baz\" 23000 " "baz" "test-mixed" #f)
            '("tests/escaped.txt" "\"foo\"" "test-escaped" #t)
            '("tests/mix-escaped.txt" "\"foo\"" "test-mix-escaped" #t)))

(define (test-with str expect name file?)
  (let ((proc (lambda (p) 
                (test-equal name 
                  expect 
                  (get-string p)))))
    (if file?
        (call-with-input-file str proc)
        (call-with-input-string str proc))))

(define (run-test tt)
  (cond
   ((null? tt) #t)
   (else
    (match (car tt)
      ((s e n f?) (test-with s e n f?)))
    (run-test (cdr tt)))))

(run-test tt)

(test-end "get-string")



