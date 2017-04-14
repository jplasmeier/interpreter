; EECS 345 Programming Project
; J. Plasmeier - jgp45@case.edu

; load the parser
(load "functionParser.scm")

; interpret - parse a file and interpret it
(define interpret
  (lambda (file)
    (interpreter (parser file) (lambda (s) s))))

; interpreter - takes a list of expressions and evaluates them
(define interpreter
  (lambda (listOfExpressions state)
    (call/cc
     (lambda return-main
       (letrec ((loop (lambda (listOfExpressions state)
                        (loop (cdr listOfExpressions) (evaluate (car listOfExpressions)
                                                                state
                                                                return-main
                                                                (lambda (e) (error "Error: continue found outside of a while body"))
                                                                (lambda (e) (error "Error: break called outside of a scoped block"))
                                                                (lambda (t e) (error "Error: throw called outside of a try block"))))
                        )))
         (loop listOfExpressions state))))))

; evaluate - takes an expression and evaluates it
(define evaluate
  (lambda (expr state return continue break throw)
    (cond
      ((null? expr) (return (get-value 'Main (call-func-expression 'main '() 'Main state)))) ; this only happens when parsing outer level so we know to return a value
      ((isReturn? expr) (return (get-value (return-body expr) state) (pop-frame state))) ; return state and value of return body
      ((isBegin? expr) (eval-begin expr state return continue break throw))
      ((isBreak? expr) (break state))
      ((isThrow? expr) (throw state (get-value (throw-body expr) state)))
      ((isIf? expr) (eval-if expr state return continue break throw))
      ((isWhile? expr) (call/cc
                        (lambda (break-here)
                          (eval-while expr state return continue break-here throw))))
      ((isContinue? expr) (continue state))
      ((isTry? expr) (eval-try expr state return continue break throw))
      ((isVariableDeclaration? expr) (declare-variable (var-name expr)
                                                       (var-value expr)
                                                       state))
      ((isVariableAssignment? expr) (assign-variable (assign-name expr) 
                                                     (assign-value expr) 
                                                     state))
      ((isFunctionCall? expr) (call-func-statement (function-call-name expr)
                                                   (function-call-params expr)
                                                   state))
      ((isFunctionDefinition? expr) (define-function expr state))
      (else (error "Reached unexpected expression while evaluating")))))

; evaluate-function - evaluates the body of a function and returns a state
(define evaluate-function
  (lambda (function-body state return)
    (letrec ((loop (lambda (function-body state return)
                     (loop (cdr function-body) (evaluate (car function-body)
                                                             state
                                                             return
                                                             (lambda (e) (error "Error: continue found outside of a while body"))
                                                             (lambda (e) (error "Error: break called outside of a scoped block"))
                                                             (lambda (t e) (error "Error: throw called outside of a try block"))))
                     )))
      (loop function-body state return)
      )))

(define call-func-expression
  (lambda (funcname parameters expr-var-name state)
    (call/cc
     (lambda (return-here) ; when this function returns
       (evaluate-function (function-body expr) 
                          state 
                          (lambda (value state) (return-here (assign-variable expr-var-name value state))) ; when this function returns, apply its value to the state
                          )))))

(define call-func-statement
  (lambda (funcname parameters state)
    (call/cc
     (lambda (return-here)
       (evaluate-function (function-body expr)
                          state
                          (lambda (value state) (return-here state)) ; when the function returns, just return the state
                          )))))