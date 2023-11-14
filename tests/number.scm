(use-modules (srfi srfi-64)
             (lexer number)
             (ice-9 popen)
             (ice-9 match))

(test-begin "number-module")

(define tt-fixnum (list
                   '(2000 #t "test-f1")
                   '(-2000 #t "test-f2")
                   '(1_000_000 #t "test-f3")
                   '(0xff #t "test-f4")
                   '(0.01 #f "test-f5")
                   '(0_000_000.01 #f "test-f6")
                   '(+2000 #t "test-f7")
                   '(0x #f "test-f8")
                   '(a #f "test-f9")
                   '(xf #f "test-f10")
                   '(0xf #t "test-f11")))

(define tt-decimal (list
                    '(200. #t "test-d1")
                    '(-200.01 #t "test-d2")
                    '(1_000.01 #t "test-d3")
                    '(0xff #f "test-d4")
                    '(0.0_100_40 #t "test-d5")
                    '(0_000_000.01 #t "test-d6")
                    '(+200.0 #t "test-d7")))

(define tt-get-number-type (list
                            '("200." decimal "test-t1")
                            '("-200" fixnum "test-t2")
                            '("1_000.01" decimal "test-t3")
                            '("0xff" fixnum "test-t4")
                            '("0.0_100_40" decimal "test-t5")
                            '("0_000_000.01" decimal "test-t6")
                            '("+200.0" decimal "test-t7")
                            '("0x" nan "test-t8")
                            '("a" nan "test-t9")
                            '("ff" nan "test-t10")))


(define (test-with e expect name proc)
  (test-equal name expect (proc e)))

(define (test-with-input-string e expect name)
  (let ((res (call-with-input-string e (lambda (p) (get-number-type p)))))
    (test-equal name expect (cadr res))))

(define (run-test tt proc)
  (cond
   ((null? tt) #t)
   (else
    (match (car tt)
      ((s e n) (test-with s e n proc)))
    (run-test (cdr tt) proc))))

(define (run-string-test tt)
  (cond
   ((null? tt) #t)
   (else
    (match (car tt)
      ((s e n) (test-with-input-string s e n)))
    (run-string-test (cdr tt)))))

(run-test tt-fixnum fixnum?)

(run-test tt-decimal decimal?)

(run-string-test tt-get-number-type)

(test-end "number-module")


