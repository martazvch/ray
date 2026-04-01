# case

See if function declaration is persistent across prompts

## part

- code
```
fn add(x, y: int = 8) -> int {
    return x + y
}
```

## part

- code
```
print add(1, 2)
```

- res
```
3
```

## part

- code
```
print add()
```

- res
```
16
```

