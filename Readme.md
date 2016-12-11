### Run File

```
cargo run example.lang
```

### Start REPL

```
cargo run
```

### Run tests

```
python tests.py
```

### Language

```
# Assignment

let a = 1

# Primitive Types

let str = "string"
let int = 1
let bool = true
let list = [1, 2, 3]
let hashmap = { "a": 1, "b": 2 }
let func = fn (x: int) { x }

# If / While

if(eq(1, a), {
  print("equal")
}, {
  print("not equal")
})

while(lt(a, 3), {
  print(("a", a))
  a = add(a, 1)
})

# Immutable data structures

eq(mget(hashmap, "a"), 1)

let new_map = insert(hashmap, "c", 3)

eq(mget(new_map, "c"), 3)

# First class functions

let add_n = fn (n: int) {
  fn (x: int) { add(x, n) }
}

let add_2 = add_n(2)
let add_3 = add_n(3)

print(add_2(1))
print(add_3(1))

# Map & Filter implementation

let map = fn (coll: vec[T], func: (T) -> R) {
    let i = 0
    let result: vec[R] = []

    while(lt(i, lsize(coll)), {
        result = push(result, func(lget(coll, i)))
        i = add(i, 1)
    })

    result
}

let mapped = map([3, 4, 5], fn(v: int) { print(("in map", v)) add(v, 5) })

let filter = fn (coll: vec[T], func: (T) -> bool) {
    let i = 0
    let result: vec[T] = []

    while(lt(i, lsize(coll)), {
        let item = lget(coll, i)
        if(func(item), {
           result = push(result, item)
           nil
        }, nil)
        i = add(i, 1)
    })

    result
}

let filtered = filter(["a", "b", "c"], fn(s: str) { print(("in filter", s)) eq(s, "b") })

print(("mapped", mapped))
print(("filtered", filtered))

# Increment all values in a map

let inc = map(pairs(hashmap), fn(t: tup[str, int]) { add(1, t1(t)) })
print(("inc", inc))

# Recursive functions

let fib = fn (x: int) {
    if(or(eq(x, 1), eq(x, 2)), {
        1
    }, {
        add(fib(sub(x, 1)), fib(sub(x, 2)))
    })
}

print(("fib 10", fib(10)))
```
