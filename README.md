# Exfo

> [!Note]
> Both the language design and compiler infrastructure around the language are currently at early development stage.

## Language Feature List 
- [X] `x86_64-linux` native assembly generation
- [X] Basic arithmetic operations on 64-bit unsigned integers (+, -, *, /)
- [X] Ordering & Logical operations with short-circuiting for
logical operations
- [X] Static scoping & shadowing (see [./examples/scoping.exfo](./examples/scoping.exfo))
- [X] C interop (including calling variadic functions)
- [X] `if-else` (see [if.exfo](./examples/if.exfo)), labeled `while`  loops (see [loops example](./examples/loops.exfo))
- [X] Pointers (see [pointers example](./examples/swap.exfo))
## Features not implemented yet
- [ ] Other platforms support (including windows)
- [ ] Pointer Arithmetic
- [ ] Some basic operations (!, %, == for `bool`, bitwise operations)
- [ ] Escape sequences, character type
- [ ] 32, 16, 8 bit integer, 64, 32-bit floats
- [ ] User types (structures, etc)
- [ ] Arrays, slices, sized strings.
- [ ] Indirect function calls
### Code & Language Semantics: 
- [ ] Global scope pre-resolution 
- [ ] Smarter type system 
- [ ] Better error messages & error recovery in parser. 


## Building (x86_64-linux) 
Please make sure that you have C compiler `cc` & GNU Assembler `as` installed on your system and available 
in `$PATH`

run
```shell
cargo build --release 
```
After compilation is done, the compiler will be located in `./target/release/exfo`

## Running tests
Exfo comes with its own "testing framework" which is located at [exfo_test crate](./exfo_test)

To run the tests, ensure that debug version of compiler is build 
```shell
cargo build --debug
```
Go to exfo_test directory, and run 
```shell
cargo run -- check
```

## Resources I found useful while developing this project 
- [bext-lang/b source code](https://github.com/bext-lang/b)