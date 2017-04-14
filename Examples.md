# Examples

```
function main() {
  var x = 10;
  var y = 20;
  var z = 30;
  var min = 0;

  if (x < y)
    min = x;
  else
    min = y;
  if (min > z)
    min = z;
  return min;
}
```

```
((function main () 
   ((var x 10) 
	(var y 20) 
	(var z 30) 
	(var min 0) 
	(if (< x y) (= min x) (= min y)) (if (> min z) (= min z)) (return min))))
```	

```
define-function expr state
	expr = (function main () ((var x 10) (var y 20) (var z 30) (var min 0) (if (< x y) (= min x) (= min y)) (if (> min z) (= min z)) (return min)))
	state = ((()()))
call-func 'main () state return
	state = (((main)((()(((var x 10) (var y 20) (var z 30) (var min 0) (if (< x y) (= min x) (= min y)) (if (> min z) (= min z)) (return min)))

```

