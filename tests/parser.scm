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
               '("(\"foo\")" #t "test-wrapped-string")))


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
;(run-test tt2 bool?)
;(run-test tt3 bool->boolean)

;(test-with-error ':a "test-not-a-bool-1" bool->boolean)
;(test-with-error ':b "test-not-a-bool-2" bool->boolean)

(test-end "parser")


