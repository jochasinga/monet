(use-modules (srfi srfi-64)
             (lexer decimal)
             (ice-9 popen))

(test-begin "get-decimal")

(define p1 (open-input-string "20.12"))
(define p2 (open-input-string "-20.12"))
(define p3 (open-input-string "+20.12"))
(define p4 (open-input-string "1_000.32_10"))
(define p5 (open-input-string "-1_000.32_10"))
(define p6 (open-input-string "+1_000.32_10"))
(define p7 (open-input-string "50."))
(define p8 (open-input-string "-50."))
(define p9 (open-input-string "+50."))
(define p10 (open-input-string "  foo bar :t \"baz\" -23_000.43 "))

(test-equal "test-unsigned" 
  20.12
  (get-decimal p1))

(test-equal "test-negative"
  -20.12
  (get-decimal p2))

(test-equal "test-positive"
  20.12
  (get-decimal p3))

(test-equal "test-unsigned-underscored"
  1000.321
  (get-decimal p4))

(test-equal "test-negative-underscored"
  -1000.321
  (get-decimal p5))

(test-equal "test-positive-underscored"
  1000.321
  (get-decimal p6))

(test-equal "test-implicit-fractional"
  50.0
  (get-decimal p7))

(test-equal "test-negative-implicit-fractional"
  -50.0
  (get-decimal p8))

(test-equal "test-positive-implicit-fractional"
  50.0
  (get-decimal p9))

(test-equal "test-mixed"
  -23000.43
  (get-decimal p10))

(test-end "get-decimal")
