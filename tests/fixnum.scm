(use-modules (srfi srfi-64)
             (lexer fixnum)
             (ice-9 popen))


(test-begin "get-fixnum")
(define p1 (open-input-string "2000"))
(define p2 (open-input-string "-2000"))
(define p3 (open-input-string "+2000"))
(define p4 (open-input-string "1_000_000"))
(define p5 (open-input-string "-1_000_000"))
(define p6 (open-input-string "+1_000_000"))
(define p7 (open-input-string "0xff"))
(define p8 (open-input-string "-0xff"))
(define p9 (open-input-string "+0xff"))
(define p10 (open-input-string "  foo bar :t \"baz\" -23_000 "))

(test-equal "test-unsigned" 
            2000
            (get-fixnum p1))

(test-equal "test-negative"
            -2000
            (get-fixnum p2))

(test-equal "test-positive"
            2000
            (get-fixnum p3))

(test-equal "test-unsigned-underscored"
            1000000
            (get-fixnum p4))

(test-equal "test-negative-underscored"
            -1000000
            (get-fixnum p5))

(test-equal "test-positive-underscored"
            1000000
            (get-fixnum p6))

(test-equal "test-unsigned-hex"
            255
            (get-fixnum p7))

(test-equal "test-negative-hex"
            #f
            (get-fixnum p8))

(test-equal "test-positive-hex"
            #f
            (get-fixnum p9))

(test-equal "test-mixed"
            -23000
            (get-fixnum p10))

(test-end "get-fixnum")



