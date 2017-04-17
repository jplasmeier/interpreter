# Header File

"I always thought header files in C were a bit silly, and here I am writing a fake one for a Scheme project." 

### Top-Level Execution

* interpret - parse a file and interpret it
	* file - a file to interpret
* interpreter - takes a list of expressions and evaluates them, eventually returning the value returned by main via call/cc
	* listOfExpressions - the list of expressions to interpret
	* state - the state of the program
* evaluate - takes an expression and evaluates it, returns state
	* expr - the current expression to evaluate
	* state - the state of the program
	* return - a continuation set at the point to return to
	* continue - a continuation set at the point to continue from
	* break - a continuation set to the point to break to
	* throw - a continuation set to the end of the current try block
* evaluate-function - evaluates the body of a function, returns state
	* listOfExpressions - the body of a function, a list of expressions
	* state
	* return - a continuation set at to the end of the function call

### Function Calls

* call-func-expression - calls a function and binds its return value to a given name
	* funcname - the name of the function
	* params - the actual parameters
	* expr-var-name - the name of the variable to bind the return value to
	* state
* call-func-statement - calls a function and ignores its return value
	* funcname
	* params
	* state

### Expression Evaluation

* eval-begin
	* expr
	* state
	* return, continue, break, throw
* eval-if
	* expr
	* state
	* return, continue, break, throw
* eval-while
	* expr
	* state
	* return, continue, break, throw
* eval-try
	* expr
	* state
	* return, continue, break, throw
* eval-finally
	* expr
	* state
	* return, continue, break, throw

### State Functions

* assign-variable - assigns a value to a variable (functions). this is done by examining the current state frame first, then the global (final) state frame. 
	* name
	* value
	* state-cont
* declare-variable - adds a variable & value to the current state
	* name
	* value
	* state-cont
* define-function - adds a function definition to the current state
	* name
	* params
	* body
	* state
* get-value-of-name - given a variable name, return its value
	* name
	* state
* get-value - returns the value resulting from the expression
	* expr
	* state
* pop-frame
	* state
* push-frame
	* state

### State Helpers/Utilities

* assign-variable-frame - assigns a value to a variable in a given frame
	* name
	* value
	* frame
* assign-variable-s - assigns a value to a variable (lists)	* name
	* value
	* state
* current-frame - return the current frame
	* state
* get-globals - return the state frame containing the globals
	* state
* frame-names - return the names on the current frame
	* state
* frame-values - return the values on the current frame

### Function Helpers

* bind-parameters - bind the parameters to the arguments in the state
	* params
	* args
	* state

### Misc Helpers

* atom?
	* expr
* isUnary?
	* expr
* member?
	* a
	* l

### Expression Parsing Helpers

* begin-body
* if-condition
* then-body
* else-body
* while-condition
* while-body
* try-body
* catch-block
* catch-body
* finally-body
* var-name
* var-value
* assign-name
* assign-value
* function-call-name
* function-call-params
* function-def-name
* function-def-params
* function-def-body
* isReturn?
* isBegin?
* isBreak?
* isThrow?
* isIf?
* isWhile?
* isContinue?
* isTry?
* isVariableDeclaration?
* isVariableAssignment?
* isFunctionCall?
* isFunctionDefinition?

