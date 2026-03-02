# Ray

**Ray** is a statically typed, interpreted language sitting somewhere between a systems language and a scripting language — safe and fast enough to be taken seriously, lightweight enough to actually enjoy writing.

It runs on its own stack-based VM with a mark-and-sweep GC, and is implemented entirely in [Zig](https://ziglang.org/).

![Pipeline Tests](https://github.com/martazvch/ray/actions/workflows/main.yml/badge.svg)

```rust
enum Shape2D {
    square: int,
    triangle: (int, int),

    fn fmt(self) -> str {
        return match self {
            .square => "[]"
            .triangle => "/\\"
        }
    }
}
enum Shape3D {
    cube: int,
    cylinder: (int, int),
    ...
}

error ShapeErr {
    unknown,
    wrongDim: int,
}

fn getShape(dim = 2, kind: str) -> ?(Shape2D|Shape3D)!ShapeErr {
    return match dim {
        2 => Shape2D.fromStr(kind)
        3 => Shape3D.fromStr(kind)
        _ @d => fail .wrongDim(d)
    }
}

fn main() {
    let shape = getShape() trap err {
        print "got error: {err}"
        return
    }

    if let s = shape {
        match s is {
            Shape2D {
                .square(side) => ...
                ...
            }
            Shape3D {
                ...
            }
        }
    }
}


```

## Why Ray

I built Ray because I wanted something that combines my favorite language features — strong typing, explicit error handling, no surprises — but interpreted, so you can embed it and script with it.

The primary use case is as an embeddable scripting language with first-class C and Zig interop, but it's general-purpose enough for standalone programs too.

## Features

- Static typing with inference — strong types without writing them everywhere
- Expression-oriented — blocks and control flow return values
- Error unions — `T!ErrorType` baked into the type system, not bolted on
- Nullable types — `?T` with explicit fallback handling, no null surprises
- Traits and structs — composition over inheritance, methods where they make sense
- Pattern matching — `when` for type-based dispatch, `match` for value matching
- Macros — AST-level metaprogramming, no preprocessor
- Zig and C interop — embed Ray or call into native code with minimal friction

---

## Documentation

The language spec is a work in progress, but covers current syntax, semantics, and design decisions.

- [Language Specification](docs/spec.md)

---

## Build from source

Requires [Zig](https://ziglang.org/) `0.15.2`.
```sh
git clone https://github.com/martazvch/ray.git
cd ray
zig build -Doptimize=ReleaseFast
```

---

## Tests

Each pipeline stage has its own tests — AST generation, analyzer IR, compiled bytecode, and VM runtime behavior. They live in the [tests/](tests/) folder.
```sh
zig build test -Dtest-mode -Dstress-gc
```
