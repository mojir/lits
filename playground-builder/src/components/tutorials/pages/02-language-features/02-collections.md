# Collections

Lits has three collection types: arrays, objects, and strings. All are immutable — every operation returns a new collection.

## Arrays

Arrays are ordered, mixed-type collections. Access elements by index:

```
let arr = [10, 20, 30, 40];
arr[2]
```

```
first([1, 2, 3])
```

```
rest([1, 2, 3])
```

## Map, Filter, Reduce

The core trio for transforming collections:

```
map([1, 2, 3, 4], -> $ * $)
```

```
filter([1, 2, 3, 4, 5, 6], even?)
```

```
reduce([1, 2, 3, 4], +, 0)
```

## More Sequence Operations

Slicing, reversing, sorting, and more:

```
slice([10, 20, 30, 40, 50], 1, 4)
```

```
reverse([1, 2, 3])
```

```
sort([3, 1, 4, 1, 5])
```

## Building Arrays

Generate arrays with `range`, `repeat`, and `push`:

```
range(5)
```

```
range(2, 10, 3)
```

```
repeat("x", 4)
```

```
push([1, 2], 3, 4)
```

## Objects

Objects are key-value maps. Use `get`, `assoc`, and `dissoc` to work with them:

```
let user = { name: "Alice", age: 30 };
get(user, "name")
```

```
let user = { name: "Alice", age: 30 };
assoc(user, "age", 31)
```

```
keys({ a: 1, b: 2, c: 3 })
```

```
vals({ a: 1, b: 2, c: 3 })
```

## Merging Objects

Combine objects with `merge` — later keys win:

```
merge({ a: 1, b: 2 }, { b: 3, c: 4 })
```

## Collection Predicates

Test properties of collections:

```
empty?([])
```

```
contains?([1, 2, 3], 1)
```

```
count("hello")
```
