(use-modules (srfi srfi-64)
             (lexer fixnum)
             (ice-9 popen)
             (ice-9 match))

(test-begin "fixnum-module")

(define tt1 (list
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

(define tt2 (list
             '("2000" 2000 "test-unsigned-string")
             '("-2000" -2000 "test-negative-string")
             '(120 120 "test-unsigned-number")
             '(-110 -110 "test-negative-number")
             '(+2 2 "test-positive-symbol")
             '(0xff 255 "test-hex-symbol")
             '(12_000_000 12000000 "test-underscored-symbol")))

(define (test-with-string s expect name proc)
  (call-with-input-string s
    (lambda (p)
      (test-equal name
        expect
        (proc p)))))

(define (run-string-test tt proc)
  (cond
   ((null? tt) #t)
   (else
    (match (car tt)
      ((s e n) (test-with-string s e n proc)))
    (run-string-test (cdr tt) proc))))

(run-string-test tt1 get-fixnum)

(define (test-with s expect name proc)
  (test-equal name expect (proc s)))

(define (run-test tt proc)
  (cond
   ((null? tt) #t)
   (else
    (match (car tt)
      ((s e n) (test-with s e n proc)))
    (run-test (cdr tt) proc))))

(run-test tt2 fixnum->number)

(test-end "fixnum-module")


