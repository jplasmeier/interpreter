; EECS 345 Programming Project
; J. Plasmeier - jgp45@case.edu

; load the parser
(load "functionParser.scm")

; interpret - parse a file and interpret it
(define interpret
  (lambda (file)
    (interpreter (parser file))))

; interpreter - takes a list of expressions and evaluates them
(define interpreter
  (lambda (listOfExpressions)
    (letrec ((loop (lambda (listOfExpressions state)
                     (loop (cdr listOfExpressions) (evaluate (car listOfExpressions)
                                                             state
                                                             (lambda (t e) (error "Error: return found outside of a function body"))
                                                             (lambda (e) (error "Error: continue found outside of a while body"))
                                                             (lambda (e) (error "Error: break called outside of a scoped block"))
                                                             (lambda (t e) (error "Error: throw called outside of a try block"))))
                     )))
      (loop listOfExpressions (lambda (s) s)))))

; evaluate - takes an expression and evaluates it
(define evaluate
  (lambda (expr state return continue break throw)
    (cond
      ((null? expr) (call-func 'main '() state return)) ; no more expressions -> call main
      ((isReturn? expr) (eval-return expr state return)) ; return 
      ((isBegin? expr) (eval-begin expr state return continue break throw))
      ((isBreak? expr) (break state))
      ((isThrow? expr) (throw state (get-value (throw-body expr) state return)))
      ((isIf? expr) (eval-if expr state return continue break throw))
      ((isWhile? expr) (call/cc
                        (lambda (break-here)
                          (eval-while expr state return continue break-here throw))))
      ((isContinue? expr) (continue state))
      ((isTry? expr) (eval-try expr state return continue break throw))
      ((isVariableDeclaration? expr) (declare-variable expr state return))
      ((isFunctionCall? expr) (eval-function-call expr state return))
      ((isFunctionDefinition? expr) (define-function expr state))
      (else (error "Reached unexpected expression while evaluating")))))