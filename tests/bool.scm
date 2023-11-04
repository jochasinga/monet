
(use-modules (srfi srfi-64)
             (lexer bool)
             (ice-9 popen)
             (ice-9 match))

(test-begin "get-bool")

(define tt (list
            '(":t" :t "test-true")
            '(":f" :f "test-false")
            '(":a" #f "test-rubbish")
            '(" foo bar 2100" #f "test-none")
            '("  foo bar :t \"baz\" -23_000 " :t "test-mixed-tokens-with-true")
            '("  foo bar \"baz\" :f " :f "test-mixed-tokens-with-false")))

(define (test-with str expect name)
  (call-with-input-string str
    (lambda (p)
      (test-equal name
        expect
        (get-bool p)))))

(define (run-test tt)
  (cond
   ((null? tt) #t)
   (else
    (match (car tt)
      ((s e n) (test-with s e n)))
    (run-test (cdr tt)))))

(run-test tt)

(test-end "get-bool")


