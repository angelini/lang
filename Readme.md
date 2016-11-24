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
func = fn (x) { x }

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

eq(get(map, "a"), 1)

new_map = insert(map, "c", 3)

eq(get(new_map, "c"), 3)

# First class functions

add_n = fn (n) {
  fn (x) { add(x, n) }
}

add_2 = add_n(2)
add_3 = add_n(3)

print(add_2(1))
print(add_3(1))

# For loop implementation

for = fn (coll, func) {
    i = 0
    ks = keys(coll)
    size = len(ks)

    while(lt(i, size), {
        key = get(ks, i)
        func(key, get(coll, key))
        i = add(i, 1)
    })
}

for([5, 6, 7, 8], fn (i, v) {
    print("i", i)
    print("v", v)
})

for({"a": 1, "b": 2}, fn (k, v) {
    print("k", k)
    print("v", v)
})
```
