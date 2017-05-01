(load "interpret.scm")

(string-append "test 1: " (if (eq? (interpret "testcases/Test4-01.txt" "A") 15) "passed" "failed"))
(string-append "test 2: " (if (eq? (interpret "testcases/Test4-02.txt" "A") 12) "passed" "failed"))
(string-append "test 3: " (if (eq? (interpret "testcases/Test4-03.txt" "A") 125) "passed" "failed"))
(string-append "test 4: " (if (eq? (interpret "testcases/Test4-04.txt" "A") 36) "passed" "failed")) 
(string-append "test 5: " (if (equal? (interpret "testcases/Test4-05.txt" "A") 54) "passed" "failed"))
(string-append "test 6: " (if (eq? (interpret "testcases/Test4-06.txt" "A") 110) "passed" "failed"))
(string-append "test 7: " (if (equal? (interpret "testcases/Test4-07.txt" "C") 26) "passed" "failed"))