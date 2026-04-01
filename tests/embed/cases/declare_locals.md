# case

See if variable declarations are persistent across prompts

## part

- code
```
let a = 65
```

## part

- code
```
print a
```

- res
```
65
```

## part

- code
```
let other = "a string"
```

## part

- code
```
print a
```

- res
```
65
```

## part

- code
```
print other
```

- res
```
a string
```

