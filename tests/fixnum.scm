(use-modules (srfi srfi-64)
             (lexer fixnum)
             (ice-9 popen)
             (ice-9 match))

(test-begin "get-fixnum")

(define tt (list
            '("2000" 2000 "test-unsigned")
            '("-2000" -2000 "test-negative")
            '("+2000" 2000 "test-positive")
            '("1_000_000" 1000000 "test-unsigned-underscored")
            '("-1_000_000" -1000000 "test-negative-underscored")
            '("+1_000_000" 1000000 "test-positive-underscored")
            '("0xff" 255 "test-unsigned-hex")
            '("-0xff" #f "test-negative-hex")
            '("+0xff" #f "test-negative-hex")
            '("  foo bar :t \"baz\" -23_000 " -23000 "test-mixed-tokens")))

(define (test-with str expect name)
  (call-with-input-string str
    (lambda (p)
      (test-equal name
        expect
        (get-fixnum p)))))

(define (run-test tt)
  (cond
   ((null? tt) #t)
   (else
    (match (car tt)
      ((s e n) (test-with s e n)))
    (run-test (cdr tt)))))

(run-test tt)

(test-end "get-fixnum")


