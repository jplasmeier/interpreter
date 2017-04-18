# Part 3 Notes 

Part 3 adds function calls. 

#### Execution Order/Flow

The `interpreter` function which loops over the parsed list of expressions no longer needs a `call/cc` for `return` because of the new execution mode. Previously, `return` would appear in the list of expressions so we needed to stop parsing and call the continuation. Now, we want `return` statements to break out of the current function call. Execution should still flow in general. 

#### Function Calls

Which continuations do function calls need? The point of passing along continuations is that if one of the keywords associated is found, the continuation is called and execution continues from where that continuation was called. When a function is called, the only continuation it needs is `return`. The rest should be reinitialized to the error states when passed in. So for things like `get-value` which may need to call functions, the only continuation which needs to be passed in is `return`. 

So when we call a function, we use return similarly to break when we evaluate a loop, or more accurately, throw, because there is an associated value. 

What should we use to actually evaluate the function? A function body is effective the same as the outer scope. However, we can't reuse `interpreter` because it returns a value, not state. We should use something similar though. 

What should the result of a function call return, exactly? Obviously, it must be some combination of state and/or value. Ideally, it would return the state of the program when the function was called with all modifications to global variables. If the function call occurred within a variable assignment, that variable should have the function's result as its value. 

```
var x = 10;
var y = 20;
var z = 30;

function foo(p) {
	z = z + 1;
	return 5;
}

var x = funcname(foo);

function main() {
	var w = 2;
	funcname(x);
	w = funcname(x);
	return w;
}
```

```
# state before function call in "var x = ..."
(((x y z funcname)(10 20 30 ($funcinfo))))

# state after function call in "var x = ..."
( ((x y z funcname)(5 20 31 ($funcinfo))) )

# state before first call in main
( ((w)(2)) ((x y z funcname)(5 20 31 ($funcinfo))) )

# state after the first call in main/before second call
( ((w)(2)) ((x y z funcname)(5 20 32 ($funcinfo))) )

# state after the second call in main
( ((w)(5)) ((x y z funcname)(5 20 33 ($funcinfo))) )
```

So how do we preserve changes to globals, while still correctly handling the return value?

No matter what, the declaration must return a state. Also, "naked" function calls (like the first call in main) must return state. So therefore function calls ought to return state. 

There are two functions for calling a function as part of an expression and as a statement. The one for calling as an expression has an extra parameter- the name to bind the return value to. When a function returns, it passes back the state and the return value. In the expression version, the return value is bound to the given name within the current frame of the state. In the statement version, the value is ignored and just the state itself is passed back.

### Functions left to implement

pop-frame
push-frame

#### Glossary

* Actual Parameter: the actual value that is passed into the method by a caller.
* Argument: the data passed into the function call.
* Expression: unit of execution that returns a value
* Formal Parameter: the identifier used in a method to stand for the value that is passed into the method by a caller.
* Statement: unit of execution that does not return a value