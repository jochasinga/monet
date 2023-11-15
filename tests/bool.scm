(use-modules (srfi srfi-64)
             (lexer bool)
             (ice-9 popen)
             (ice-9 match))

(test-begin "bool")

(define tt1 (list
            '(":t" :t "test-true")
            '(":f" :f "test-false")
            '(":a" #f "test-rubbish")
            '(" foo bar 2100" #f "test-none")
            '("  foo bar :t \"baz\" -23_000 " :t "test-mixed-tokens-with-true")
            '("  foo bar \"baz\" :f " :f "test-mixed-tokens-with-false")))

(define tt2 (list
             '(:t #t "test-bool?-true")
             '(:f #t "test-bool?-false")
             '(:a #f "test-bool?-no-1")
             '(:b #f "test-bool?-no-2")
             '(":t" #t "test-bool?-string-true")
             '(":f" #t "test-bool?-string-false")
             '(":a" #f "test-bool?-string-no-1")
             '(":b" #f "test-bool?-string-no-2")))

(define tt3 (list
             '(:t #t "test-1")
             '(:f #f "test-2")))

(define (test-with-string str expect name proc)
  (call-with-input-string str
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

(define (test-with s expect name proc)
  (test-equal name expect (proc s)))

(define (test-with-error s name proc)
  (test-error name #t (proc s)))

(define (run-test tt proc)
  (cond
   ((null? tt) #t)
   (else
    (match (car tt)
      ((s e n) (test-with s e n proc)))
    (run-test (cdr tt) proc))))

(run-string-test tt1 get-bool)
(run-test tt2 bool?)
(run-test tt3 bool->boolean)

(test-with-error ':a "test-not-a-bool-1" bool->boolean)
(test-with-error ':b "test-not-a-bool-2" bool->boolean)

(test-end "bool")


