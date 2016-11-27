### Run File

```
cargo run example.lang
```

### Start REPL

```
cargo run
```

### Language

```
# Assignment

a = 1

# Primitive Types

str = "string"
int = 1
bool = true
list = [1, 2, 3]
map = { "a": 1, "b": 2 }
func = fn (x: int) { x }

# If / While

if(eq(1, a), {
  print("equal")
}, {
  print("not equal")
})

while(lt(a, 3), {
  print("a", a)
  a = add(a, 1)
})

# Immutable data structures

eq(mget(map, "a"), 1)

new_map = insert(map, "c", 3)

eq(mget(new_map, "c"), 3)

# First class functions

add_n = fn (n: int) {
  fn (x: int) { add(x, n) }
}

add_2 = add_n(2)
add_3 = add_n(3)

print(add_2(1))
print(add_3(1))

# Map & Filter implementation

map = fn (coll: vec[T], func: (T) -> R) {
    i = 0
    result: vec[R] = []

    while(lt(i, lsize(coll)), {
        result = push(result, func(lget(coll, i)))
        i = add(i, 1)
    })

    result
}

mapped = map([3, 4, 5], fn(v: int) { print("in map", v) add(v, 5) })

filter = fn (coll: vec[T], func: (T) -> bool) {
    i = 0
    result: vec[T] = []

    while(lt(i, lsize(coll)), {
        item = lget(coll, i)
        if(func(item), {
           result = push(result, item)
           nil
        }, nil)
        i = add(i, 1)
    })

    result
}

filtered = filter(["a", "b", "c"], fn(s: str) { print("in filter", s) eq(s, "b") })

print("mapped", mapped)
print("filtered", filtered)
```
