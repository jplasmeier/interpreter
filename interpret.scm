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
      ((isBegin? expr) (pop-frame (eval-begin (begin-body expr) (push-frame state) return continue break throw)))
      ((isBreak? expr) (break state))
      ((isThrow? expr) (throw state (get-value (throw-body expr) state)))
      ((isIf? expr) (eval-if expr state return continue break throw))
      ((isWhile? expr) (call/cc
                        (lambda (break-here)
                          (eval-while expr state return continue break-here throw))))
      ((isContinue? expr) (continue state))
      ((isTry? expr) (eval-try expr state return continue break throw))
      ((isVariableDeclaration? expr) (declare-variable (var-name expr)
                                                       (if (null? (var-value expr))
                                                           ('())
                                                           (get-value (var-value expr) state))
                                                       state))
      ((isVariableAssignment? expr) (assign-variable (assign-name expr) 
                                                     (get-value (assign-value expr) state) 
                                                     state))
      ((isFunctionCall? expr) (call-func-statement (function-call-name expr)
                                                   (function-call-params expr)
                                                   state))
      ((isFunctionDefinition? expr) (define-function 
                                      (function-def-name name)
                                      (function-def-params params) 
                                      (function-def-body body) 
                                      state))
      (else (error "Reached unexpected expression while evaluating")))))

; evaluate-function - evaluates the body of a function and returns a state
(define evaluate-function
  (lambda (listOfExpressions state return)
    (letrec ((loop (lambda (listOfExpressions state return)
                     (loop (cdr listOfExpressions) (evaluate (car listOfExpressions)
                                                             state
                                                             return
                                                             (lambda (e) (error "Error: continue found outside of a while body"))
                                                             (lambda (e) (error "Error: break called outside of a scoped block"))
                                                             (lambda (t e) (error "Error: throw called outside of a try block"))))
                     )))
      (loop function-body state return)
      )))

; call-func-expression - calls a function and binds its return value to a given name
(define call-func-expression
  (lambda (funcname params expr-var-name state)
    (call/cc
     (lambda (return-here) ; when this function returns
       (evaluate-function (function-body (get-value-of-name funcname state)) 
                          (bind-parameters params
                                           (function-args (get-value-of-name funcname state))
                                           state)
                          (lambda (value state) (return-here (assign-variable expr-var-name value state))) ; when this function returns, apply its value to the state
                          )))))

; call-func-statement - calls a function and ignores its return value
(define call-func-statement
  (lambda (funcname params state)
    (call/cc
     (lambda (return-here)
       (evaluate-function (function-body (get-value-of-name funcname state))
                          (bind-parameters params
                                           (function-args (get-value-of-name funcname state))
                                           state)
                          (lambda (value state) (return-here state)) ; when the function returns, just return the state
                          )))))

; Expression Evaluation

(define eval-begin
  (lambda (expr state return continue break throw)
    (if (null? expr)
        state
        (eval-begin (cdr expr) 
                    (evaluate (car expr) state return continue break throw)
                    return
                    continue
                    break
                    throw))))

(define eval-if
  (lambda (expr state return continue break throw)
    (if (get-value (if-condition expr) state)
        (evaluate (then-body) state return continue break throw)
        (if (null? (else-body expr))
            state
            (evaluate (else-body expr) state return continue break throw)))))

(define eval-while
  (lambda (expr state return continue break throw)
    (if (get-value (while-condition expr) state)
        (eval-while expr
                    (call/cc
                     (lambda (continue-here)
                       (evaluate (while-body expr) state return continue-here break throw)))
                    return
                    continue
                    break
                    throw)
        state)))

(define eval-try
  (lambda (expr state return continue break throw)
    (if (null? catch-block)
        (eval-finally (finally-body expr)
                      (eval-begin (try-body expr)
                                  (push-frame state)
                                  return
                                  continue ; do these need to also have a frame pushed? 
                                  break    ; on part 2 they did but idk if needed
                                  throw)
                      return
                      continue ; same deal here
                      break
                      throw)
        (eval-finally (finally-body expr)
                      (push-frame (call/cc
                                   (lambda (throw-here)
                                     (pop-frame (eval-begin (try-body expr)
                                                            (push-frame state)
                                                            return
                                                            continue ; again
                                                            break
                                                            (lambda (t e) (throw-here (eval-begin (catch-body expr)
                                                                                                  (get-value e t)
                                                                                                  return
                                                                                                  continue ; again
                                                                                                  break
                                                                                                  throw))))))))
                      return
                      continue ; again
                      break
                      throw))))
  
; State Functions

; assign-variable - assigns a value to a variable
(define assign-variable
  (lambda (name value state)
    ()))

; declare-variable - adds a variable & value to the current state
(define declare-variable
  (lambda (name value state)
    ()))

; define-function - adds a function definition to the current state
(define define-function
  (lambda (name params body state)
    (declare-variable name 
                      (cons params 
                            (cons body '())) 
                      state)))

; get-value-of-name - given a variable name, return its value
(define get-value-of-name
  (lambda (name state)
    ()))

; get-value - returns the value resulting from the expression
(define get-value
  (lambda (expr state)
    (cond
      ((number? expr) expr)
      ((eq? 'true expr) #t)
      ((eq? 'false expr) #f)
      ((atom? expr) (get-value-of-name expr state))
      ((eq? (operator expr) '+) (+ (get-value (operand1 expr) state)
                                   (get-value (operand2 expr) state)))
      ((eq? (operator expr) '-) (if (isUnary? expr)
                                    (- (get-value (operand1 expr) state))
                                    (- (get-value (operand1 expr) state)
                                       (get-value (operand2 expr) state))))
      ((eq? (operator expr) '*) (* (get-value (operand1 expr) state)
                                   (get-value (operand2 expr) state)))
      ((eq? (operator expr) '/) (floor (/ (get-value (operand1 expr) state)
                                   (get-value (operand2 expr) state))))
      ((eq? (operator expr) '%) (modulo (get-value (operand1 expr) state)
                                   (get-value (operand2 expr) state)))
      ((eq? (operator expr) '>) (> (get-value (operand1 expr) state)
                                   (get-value (operand2 expr) state)))
      ((eq? (operator expr) '<) (< (get-value (operand1 expr) state)
                                   (get-value (operand2 expr) state)))
      ((eq? (operator expr) '>=) (>= (get-value (operand1 expr) state)
                                     (get-value (operand2 expr) state)))
      ((eq? (operator expr) '<=) (<= (get-value (operand1 expr) state)
                                     (get-value (operand2 expr) state)))
      ((eq? (operator expr) '==) (eq? (get-value (operand1 expr) state)
                                      (get-value (operand2 expr) state)))
      ((eq? (operator expr) '!=) (not (eq?(get-value (operand1 expr) state)
                                          (get-value (operand2 expr) state))))
      ((eq? (operator expr) '||) (or (get-value (operand1 expr) state)
                                      (get-value (operand2 expr) state)))
      ((eq? (operator expr) '&&) (and (get-value (operand1 expr) state)
                                      (get-value (operand2 expr) state)))
      ((eq? (operator expr) '!) (not (get-value (operand1 expr) state)))
      (else (error "Error gettting value.")))))

(define pop-frame
  (lambda (state)
    ()))

(define push-frame
  (lambda (state)
    ()))

; Function Helpers 
  
; bind-parameters - bind the parameters to the arguments in the state
(define bind-parameters
  (lambda (params args state)
    (cond
      ((and (null? params) (null? args)) state)
      ((null? params) (error "Too many arguments, not enough parameters!"))
      ((null? args) (error "Too many parameters, not enough arguments!"))
      (else (declare-variable (car params) (get-value (car args) state) (bind-parameters (cdr params) (cdr args) state))))))
      
; Misc Helpers

(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))

(define isUnary?
  (lambda (expr)
    (and (not (null? (cdr expr))) (null? (cddr expr)))))

; Expression Parsing Helpers

(define begin-body cdr)
(define if-condition cadr)
(define then-body caddr)
(define else-body (lambda (ex) (if (null? (cdddr ex)) '() (cadddr ex))))
(define while-condition cadr)
(define while-body caddr)
(define try-body cadr)
(define catch-block caddr)
(define catch-body (lambda (ex) (caddr (caddr ex))))
(define finally-body cadddr)
(define var-name cadr)
(define var-value (lambda (expr state) (get-value (caddr expr) state)))
(define assign-name cadr)
(define assign-value (lambda (expr state) (get-value (caddr expr) state)))
(define function-call-name cadr)
(define function-call-params cddr)
(define function-def-name cadr)
(define function-def-params caddr)
(define function-def-body cadddr)

(define operator car)
(define operand1 cadr)
(define operand2 caddr)

(define isReturn? (lambda (expr) (eq? (operator expr) 'return)))
(define isBegin? (lambda (expr) (eq? (operator expr) 'begin)))
(define isBreak? (lambda (expr) (eq? (operator expr) 'break)))
(define isThrow? (lambda (expr) (eq? (operator expr) 'throw)))
(define isIf? (lambda (expr) (eq? (operator expr) 'if)))
(define isWhile? (lambda (expr) (eq? (operator expr) 'while)))
(define isContinue? (lambda (expr) (eq? (operator expr) 'continue)))
(define isTry? (lambda (expr) (eq? (operator expr) 'try)))
(define isVariableDeclaration? (lambda (expr) (eq? (operator expr) 'var)))
(define isVariableAssignment? (lambda (expr) (eq? (operator expr) '=)))
(define isFunctionCall? (lambda (expr) (eq? (operator expr) 'funcall)))
(define isFunctionDefinition? (lambda (expr) (eq? (operator expr) 'function)))