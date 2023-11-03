(use-modules (srfi srfi-64)
             (lexer string)
             (ice-9 popen))


(test-begin "get-string")

(define p1 (open-input-string "\"foo\""))
(define p2 (open-input-string ":t"))
(define p3 (open-input-file "tests/escaped.txt"))
(define p4 (open-input-string "  foo bar :t \"baz\" -23_000 "))
(define p5 (open-input-file "tests/mix-escaped.txt"))

(test-equal "test-string" 
  "foo"
  (get-string p1))

(test-equal "test-bool"
  #f
  (get-string p2))

(test-equal "test-escaped"
  "\"foo\""
  (get-string p3))

(test-equal "test-mixed"
  "baz"
  (get-string p4))

(test-equal "test-mix-escaped"
  "\"foo\""
  (get-string p5))

(close-port p1)
(close-port p2)
(close-port p3)
(close-port p4)
(close-port p5)

(test-end "get-string")



