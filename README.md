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
- [ ] Other platforms support
- [ ] Pointer Arithmetic
- [ ] Some basic operations (%)
- [ ] Character type
- [ ] 32, 16, 8 bit integer, 64, 32-bit floats
- [ ] User types (structures, etc)
- [ ] Arrays, slices, sized strings.
- [ ] Indirect function calls
### Code & Language Semantics: 
- [ ] Global scope pre-resolution 
- [ ] Smarter type system 
- [ ] Better error messages & error recovery in parser & resolver. 

## External Dependencies 
- [Rust programming language](https://rust-lang.org/) & Cargo (package manager for Rust).
- [GNU Compiler Collection (GCC)](https://gcc.gnu.org/) or [MinGW](https://www.mingw-w64.org/).
- (Optional) [Wine](https://www.winehq.org/) if you are willing to cross-test for Windows while being on Linux.

## Building (x86_64-linux) 
Please make sure that you have C compiler `cc` & GNU Assembler `as` installed on your system and available 
in `$PATH`
### Compilation
run
```shell
cargo build --release 
```
After compilation is done, the compiler will be located in `./target/release/exfo`

## Building (x86_64-windows)
Please make sure that you have MinGW software package installed and available in `$PATH`
### Compilation
run
```shell
cargo build --release 
```
After compilation is done, the compiler will be located in `.\target\release\exfo.exe`

## Usage 
Basic compiler usage is: 
```shell
exfo <file.exfo> -o <output>
```
(replace `exfo` by executable path of the compiler)

for more details, run `exfo --help`

## Running tests
Exfo comes with its own "testing framework" which is located at [exfo_test crate](./exfo_test)

To run the tests, ensure that debug version of compiler is built 
```shell
cargo build --debug # in exfo/
```
Go to exfo_test directory, and run 
```shell
cargo run -- check # in exfo/exfo_test
```
If you want to add a test, put the test code into `exfo_test/tests` folder and run 
```shell
cargo run -- record # or `record all` if you want to override all test results (not recommended unless you change a test).
```
For more details, invoke `cargo run -- help` inside exfo/exfo_test 
directory

## Resources I found useful while developing this project 
- [bext-lang/b source code](https://github.com/bext-lang/b)