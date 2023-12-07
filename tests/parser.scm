(use-modules (srfi srfi-64)
             (parser)
             (ice-9 popen)
             (ice-9 match))

(test-begin "parser")

(define terms (list
               '("1" #t "test-fixnum")
               '("1_000_000" #t "test-underscored-fixnum")
               '("1.20" #t "test-decimal")
               '("1_000.2_0_0_0" #t "test-underscored-decimal")
               '("\"foo\"" #t "test-string")
               '("(1)" #t "test-wrapped-fixnum")
               '("(12_000_000)" #t "test-wrapped-underscored-fixnum")
               '("(1.20)" #t "test-wrapped-decimal")
               '("(1_000.1_000_001)" #t "test-wrapped-underscored-decimal")
               '("(\"foo\")" #t "test-wrapped-string")
               '("1 + 1" #f "test-addition-expr")
               '("\"hello \" + \"world\"" #f "test-string-append-expr")
               '("1_000_000 / 5" #f "test-division-expr")
               '("(10 * (2 + 3))" #f "test-double-wrapped-arithmetic-expr")))

(define exprs (list
               '("(1 + 2)" #t "test-addition")
               '("(3 - 2)" #t "test-subtraction")
               '("(10 / 5)" #t "test-division")
               '("(3 * 5)" #t "test-multiplication")
               '("(1 + 3 * 2)" #t "test-continuous-arith")
               '("(1 + (3 * 20_000))" #t "test-complex-arith-1")
               '("((120_000_000 / 1000) * 2)" #t "test-complex-arith-2")
               '("(1000)" #t "test-parenwrapped-term")
               '("0xff" #t "test-hex-fixnum-term")
               '("1 + 2" #f "test-addition-without-parens")
               '("3 - 2" #f "test-subtraction-without-parens")
               '("10 / 5" #f "test-division-without-parens")
               '("3 * 5" #f "test-multiplication-without-parens")
               '("12 * (1 / 4)" #f "test-complex-arith-2")))

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

(run-string-test terms (lambda (e) (term? (parse e))))
(run-string-test exprs (lambda (e) (expr? (parse e))))
;(run-test tt2 bool?)
;(run-test tt3 bool->boolean)

;(test-with-error ':a "test-not-a-bool-1" bool->boolean)
;(test-with-error ':b "test-not-a-bool-2" bool->boolean)

(test-end "parser")


