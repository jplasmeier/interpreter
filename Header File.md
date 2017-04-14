## Header File

```
"I always thought header files in C were a bit silly, and here I am writing a fake one for a Scheme project." 
- J. Plasmeier
```

### Top-Level Execution

* interpret - parse a file and interpret it
	* file - a file to interpret
* interpreter - takes a list of expressions and evaluates them
	* listOfExpressions - the list of expressions to interpret
* evaluate - takes an expression and evaluates it
	* expr - the current expression to evaluate
	* state - the state of the program
	* return - a continuation set at the point to return to
	* continue - a continuation set at the point to continue from
	* break - a continuation set to the point to break to
	* throw - a continuation set to the end of the current try block

### Expression Evaluation

* eval-return
	* expr
	* state
	* return
* eval-begin
	* expr
	* state
	* return
	* continue
	* break
	* throw
* eval-if
	* expr
	* state
	* return
	* continue
	* break
	* throw
* eval-while
	* expr
	* state
	* return
	* continue
	* break
	* throw
* eval-try
	* expr
	* state
	* return
	* continue
	* break
	* throw
* eval-function-call
	* expr
	* state
	* return

### State Operations

* declare-variable - adds a variable & value to the current state
	* expr
	* state
	* return
* define-function - adds a function definition to the current state
	* expr
	* state
* 	

### Scoped Blocks

### Function Calls

* call-func

### Expression Parsing

* isReturn?
* isBegin?
* isBreak?
* isThrow?
* isIf?
* isWhile?
* isContinue?
* isTry?
* isVariableDeclaration?
* isFunctionCall?
* isFunctionDefinition?

