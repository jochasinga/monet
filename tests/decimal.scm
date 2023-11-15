(use-modules (srfi srfi-64)
             (lexer decimal)
             (ice-9 popen)
             (ice-9 match))

(test-begin "decimal-module")

(define tt1 (list
             '("20.12" 20.12 "test-unsigned")
             '("-20.12" -20.12 "test-negative")
             '("+20.12" 20.12 "test-positive")
             '("1_000.32_10" 1000.321 "test-unsigned-underscored")
             '("-1_000.32_10" -1000.321 "test-negative-underscored")
             '("+100_0.3_2_1_0" 1000.321 "test-positive-underscored")
             '("50." 50.0 "test-unsigned-implicit-fractional")
             '("-50." -50.0 "test-negative-implicit-fractional")
             '("+50." 50.0 "test-positive-implicit-fractional")
             '("  foo bar :t \"baz\" -23_000.43 " -23000.43 "test-mixed-tokens")
             '(":t foo bar" #f "test-none")))

(define tt2 (list
             '("20.12" 20.12 "test-unsigned-string")
             '("-20.12" -20.12 "test-negative-string")
             '("+20.12" 20.12 "test-positive-string")
             '(1_000.32_10 1000.321 "test-unsigned-underscored-symbol")
             '(-1_000.32_10 -1000.321 "test-negative-underscored-symbol")
             '(+100_0.3_2_1_0 1000.321 "test-positive-underscored-symbol")
             '(50. 50.0 "test-unsigned-implicit-fractional-symbol")
             '(-50. -50.0 "test-negative-implicit-fractional-symbol")
             '(+50. 50.0 "test-positive-implicit-fractional-symbol")))

(define (test-with-string str exp name)
  (call-with-input-string str 
    (lambda (p)
      (test-equal name
        exp
        (get-decimal p)))))

(define (run-string-test tt)
  (cond
   ((null? tt) #t)
   (else
    (match (car tt)
      ((s e n) (test-with-string s e n)))
    (run-string-test (cdr tt)))))

(run-string-test tt1)

(define (test-with e exp name)
  (test-equal name exp (decimal->number e)))

(define (run-test tt)
  (cond
   ((null? tt) #t)
   (else
    (match (car tt)
      ((s e n) (test-with s e n)))
    (run-test (cdr tt)))))



(test-end "decimal-module")
