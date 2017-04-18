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
     (lambda (return-main)
       (letrec ((loop (lambda (listOfExpressions state)
                        (loop (safecdr listOfExpressions) (evaluate (safecar listOfExpressions)
                                                                state
                                                                return-main
                                                                (lambda (e) (error "Error: continue found outside of a while body"))
                                                                (lambda (e) (error "Error: break called outside of a scoped block"))
                                                                (lambda (t e) (error "Error: throw called outside of a try block"))))
                        )))
         (loop listOfExpressions state))))))

(define safecdr
  (lambda (l)
    (if (null? l)
        '()
        (cdr l))))

(define safecar
  (lambda (l)
    (if (null? l)
        '()
        (car l))))

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
      ((isVariableDeclaration? expr) (eval-declare-variable expr state))
      ((isVariableAssignment? expr) (eval-assign-variable expr state))
      ((isFunctionCall? expr) (call-func-statement (function-call-name expr)
                                                   (function-call-params expr)
                                                   state))
      ((isFunctionDefinition? expr) (define-function 
                                      (function-def-name expr)
                                      (function-def-params expr) 
                                      (function-def-body expr) 
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
                                                             (lambda (t e) (error "Error: throw called outside of a try block"))) return)
                     )))
      (loop listOfExpressions state return)
      )))

; call-func-expression - calls a function and binds its return value to a given name
(define call-func-expression
  (lambda (funcname params expr-var-name state)
    (call/cc
     (lambda (return-here) ; when this function returns
       (evaluate-function (function-ref-body (get-value-of-name funcname state)) 
                          (bind-parameters params
                                           (function-ref-args (get-value-of-name funcname state))
                                           (push-frame state))
                          (lambda (value state) (return-here (declare-variable expr-var-name value state))) ; when this function returns, apply its value to the state
                          )))))

; call-func-statement - calls a function and ignores its return value
(define call-func-statement
  (lambda (funcname params state)
    (call/cc
     (lambda (return-here)
       (evaluate-function (function-ref-body (get-value-of-name funcname state))
                          (bind-parameters params
                                           (function-ref-args (get-value-of-name funcname state))
                                           (push-frame state))
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

(define eval-declare-variable
  (lambda (expr state)
    (if (and (not (atom? (var-value-term expr))) (eq? (car (var-value-term expr)) 'funcall))
        (call-func-expression (function-call-name (var-value-term expr))
                              (function-call-params (var-value-term expr))
                              (var-name expr)
                              state)
        (declare-variable (var-name expr)
                          (var-value expr state)
                          state))))

(define eval-assign-variable
  (lambda (expr state)
    (if (and (not (atom? (assign-value-term expr))) (eq? (car (assign-value-term expr)) 'funcall))
        (call-func-expression (function-call-name (assign-value-term expr))
                              (function-call-params (assign-value-term expr))
                              (assign-name expr)
                              state)
        (assign-variable (assign-name expr) 
                         (assign-value expr state) 
                         state))))

(define eval-if
  (lambda (expr state return continue break throw)
    (if (get-value (if-condition expr) state)
        (evaluate (then-body expr) state return continue break throw)
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

; assign-variable - assigns a value to a variable by wrapping the actual call in a function to preserve the continuation
; this is done by examining the current state frame first, then the global (final) state frame. 
(define assign-variable
  (lambda (name value state-cont)
    (lambda (s) (assign-variable-s name value (state-cont s)))))

; declare-variable - adds a variable & value to the current state
(define declare-variable
  (lambda (name value state-cont)
    (lambda (s) (declare-variable-s name value (state-cont s)))))

; define-function - adds a function definition to the current state
(define define-function
  (lambda (name params body state)
    (declare-variable name 
                      (cons params 
                            (cons body '())) 
                      state)))

; get-value-of-name - given a variable name, return its value
; check the current state frame and globals
(define get-value-of-name
  (lambda (name state)
    (get-value-of-name-s name (state e-s))))

; pop-frame - pop a frame off of the state, return the new state
(define pop-frame
  (lambda (state-cont)
    (lambda (s) (pop-frame-s (state-cont s)))))

; push-frame - push a new empty frame on to the state
(define push-frame
  (lambda (state-cont)
    (lambda (s) (push-frame-s (state-cont s)))))

; State Utilities

; assign-variable-s - assigns the value to a variable by treating state like a list and not a function
(define assign-variable-s
  (lambda (name value state)
    (cond
      ((member? name (frame-names (current-frame state))) (cons (assign-variable-frame name value (current-frame state))
                                                                (cdr state)))
      ((member? name (frame-names (get-globals state))) (append (get-locals state)
                                                                (cons (assign-variable-frame name value (get-globals state))
                                                                      '())))
      (else (error "Attempted to assign to an undeclared variable.")))))

; assign-variable-frame - assigns the value to a variable on a given frame only
(define assign-variable-frame
  (lambda (name value frame)
    (cond
      ((eq? (car (frame-names frame)) name) (cons (frame-names frame) 
                                                  (cons (cons value 
                                                              (cdr (frame-values frame))) 
                                                        '())))
      (else (cons (frame-names frame)
                  (cons (cons (car (frame-values frame))
                              (frame-values (assign-variable-frame name value (cons (cdr (frame-names frame))
                                                                                    (cons (cdr (frame-values frame))
                                                                                          '())))))
                        '()))))))

; declare-variable - adds a variable & value to the current state
(define declare-variable-s
  (lambda (name value state)
    (cons (declare-variable-frame name value (current-frame state)) (cdr state))))

; declare-variable-frame - adds a variable and value to a given frame
(define declare-variable-frame
  (lambda (name value frame)
    (if (member? name (frame-names frame))
        (error "This name is already in use.")
        (cons (cons name (frame-names frame))
              (cons (cons value (frame-values frame))
                    '())))))

; get-value-of-name-s - list version of get-value-of-name
(define get-value-of-name-s
  (lambda (name state)
    (cond
      ((member? name (frame-names (current-frame state))) (get-value-of-name-frame name (current-frame state)))
      ((member? name (frame-names (get-globals state))) (get-value-of-name-frame name (get-globals state)))
      (else "This name does not exist in the current frame or globals"))))

; get-value-of-name-frame - get the value of a name from a given frame
(define get-value-of-name-frame
  (lambda (name frame)
    (if (eq? name (car (frame-names frame)))
        (car (frame-values frame))
        (get-value-of-name-frame name (cons (cdr (frame-names frame))
                                            (cons (cdr (frame-values frame)) '()))))))

; get-globals - return the (final) state frame containing the globals
(define get-globals
  (lambda (state)
    (if (null? (cdr state)) 
        (car state)
        (get-globals (cdr state)))))

; get-locals - return the state frames on top of the globals ("locals")
(define get-locals
  (lambda (state)
    (if (null? (cdr state))
        '()
        (cons (car state) (get-locals (cdr state))))))

; pop-frame-s - pop a frame off of the state, return the new state
(define pop-frame-s
  (lambda (state)
    (cdr state)))

; push-frame-s - push a new empty frame on to the state
(define push-frame-s
  (lambda (state)
    (cons '(()()) state)))

; State Helpers 

(define current-frame car)
(define frame-names car)
(define frame-values cadr)

; Function Helpers 

; bind-parameters - bind the parameters to the arguments in the state
(define bind-parameters
  (lambda (params args state)
    (cond
      ((and (null? params) (null? args)) state)
      ((null? params) (error "Too many arguments, not enough parameters!"))
      ((null? args) (error "Too many parameters, not enough arguments!"))
      (else (declare-variable (car params) (get-value (car args) state) (bind-parameters (cdr params) (cdr args) state))))))

; Misc Helpers/Utilities

; get-value - returns the value resulting from the expression
(define get-value
  (lambda (expr state)
    (cond
      ((null? expr) '())
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

(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))

(define isUnary?
  (lambda (expr)
    (and (not (null? (cdr expr))) (null? (cddr expr)))))

(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((eq? (car l) a) #t)
      (else (member? a (cdr l))))))

(define e-s '((()())))

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
(define var-value-term caddr)
(define var-name cadr)
(define var-value (lambda (expr state) (get-value (var-value-term expr) state)))
(define assign-name cadr)
(define assign-value-term caddr)
(define assign-value (lambda (expr state) (get-value (assign-value-term expr) state)))
(define function-call-name cadr)
(define function-call-params cddr)
(define function-def-name cadr)
(define function-def-params caddr)
(define function-def-body cadddr)
(define function-ref-body cadr)
(define function-ref-args car)
(define return-body cadr)

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