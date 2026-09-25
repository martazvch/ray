# case

See if variable declarations are persistent across prompts

## part

- code
```
a := 65
```

## part

- code
```
print(a)
```

- res
```
65
```

## part

- code
```
other := "a string"
```

## part

- code
```
print(a)
```

- res
```
65
```

## part

- code
```
print(other)
```

- res
```
a string
```

