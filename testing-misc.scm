; Interpreter Tests
; Lord Help Us All

(load "interpret.scm")

(define test-from-string
  (lambda (l-ex state expected)
    (if (eq? (interpreter l-ex state) expected)
        (display "\nPass!")
        (dispaly "\nFail!!!"))))

(define test-from-file
  (lambda (file expected)
    (if (eq? (interpret file) expected)
        (display "\nPass!")
        (display "\nFailure!!!"))))

; mess around with globals from main
(test-from-file "testcases/MyTest1.txt" 4)
; mess around with globals in another function
(test-from-file "testcases/MyTest2.txt" 8)
; function calls in parameters
(test-from-file "testcases/MyTest3.txt" 10)
; function without return
(test-from-file "testcases/MyTest4.txt" 9)
; variable in function call
(test-from-file "testcases/MyTest5.txt" 10)
; function call in return
(test-from-file "testcases/MyTest6.txt" 15)
; function call in global declaration
(test-from-file "testcases/MyTest7.txt" 9)
; function call with if statement called in global declaration
(test-from-file "testcases/MyTest8.txt" 15)
; function call in declaration inside main
;(test-from-file "testcases/MyTest9.txt" 6)
; while loops
(test-from-file "testcases/MyTest10.txt" 0)

