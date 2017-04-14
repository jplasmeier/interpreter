# Part 3 Notes 



#### Execution Order/Flow

The `interpreter` function which loops over the parsed list of expressions no longer needs a `call/cc` for `return` because of the new execution mode. Previously, `return` would appear in the list of expressions so we needed to stop parsing and call the continuation. Now, we want `return` statements to break out of the current function call. Execution should still flow in general. 

#### Function Calls

Which continuations do function calls need? The point of passing along continuations is that if one of the keywords associated is found, the continuation is called and execution continues from where that continuation was called. When a function is called, the only continuation it needs is `return`. The rest should be reinitialized to the error states when passed in. So for things like `get-value` which may need to call functions, the only continuation which needs to be passed in is `return`. 

So when we call a function, we use return similarly to break when we evaluate a loop, or more accurately, throw, because there is an associated value. 


#### Glossary

* Actual Parameter: the actual value that is passed into the method by a caller.
* Argument: the data passed into the function call.
* Expression: unit of execution that returns a value
* Formal Parameter: the identifier used in a method to stand for the value that is passed into the method by a caller.
* Statement: unit of execution that does not return a value