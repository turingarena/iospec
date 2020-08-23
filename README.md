# Turingarena IOspec

A language to specify input/output format, and tools to automate I/O validation, parser generation, and more.

**NB**: Turingarena IOspec is work-in-progress. Features still to be implemented are marked with "TODO" in this README.

## What is Turingarena IOspec?

When defining a programming problem based on file-like input and output, one needs to define a contract with the problem solver, where the following are specified:

* format of the input,
* assumption on the input which are guaranteed to hold,
* format of the output,
* assumption on the output which are required to hold for the output to be accepted.

Turingarena IOspec allows to specify formally the format of input and output, and some of the assumptions on them (as long as some conventions are followed, which are very common in [IOI](https://ioinformatics.org/)-like problems).

Once the format is specifies, the following tasks can be automated:

* generating:
    * in many different programming languages (TODO: only *C++* at the moment),
        * *skeleton* code which parses the input and formats the output, providing an abstraction over the I/O (e.g., playing the role of "grader" in [CMS](https://cms-dev.github.io/) problem in Italian format),
        * *template* code with the functions to be implemented by the problem solver, which, once filled by the problem solver, without managing the I/O, and combined with the skeleton, gives a working solution (TODO),
    * a human-readable description of the input/output format and its assumptions, in many output formats (TODO), 
* validating (TODO):
    * input files,
    * output files, given their respective input,
    * input and output streams on-the-fly, for interactive problems,
* canonicalizing (TODO):
    * input files or streams, so that the input generator does not need to care about proper formatting (e.g., whitespace),
    * output files or streams, so that the output checker can assume that the output is properly formatted (e.g., whitespace),
* converting input/output files:
    * from textual to binary and vice-versa (TODO),
    * from plain (textual or binary) to structured (e.g., JSON) and vice-versa (TODO),

## The specification language

In IOspec, the format is described in a language *similar to a programming language*, which defines, *from the perspective of the problem solver*:

* how the input should be parsed,
* how and when solution code needs to be executed, to compute the output,
* how the output should be formatted,
* how and when assumptions should be checked (TODO).

An example is worth a thousand words. Here is the I/O specification for the problem of finding a cycle in a graph encoded as an edge list.

```
read N: n32, M: n32;

assume 2 <= N && N <= 100_000;
assume 1 <= M && M <= 1_000_000;

for i upto M {
    read A[i]: n32, B[i]: n32;

    assume 0 <= A[i] && A[i] < B[i] && B[i] < N;
}

call find_cycle(N, M, A, B) -> L: n32; // length of the cycle
assert 2 <= L && L <= N;

write L;

for i upto L {
    call get_cycle_node(i) -> u: n32;
    assert 0 <= u && u < N;

    write u;
}
```

The language supports many features common of programming languages, but it also has many restrictions which allow it to be used to automate some tasks (which would otherwise be harder or impossible), and at the same time making it more compact.

Features:

* scalar types:
    * booleans: `bool` (either true, represented as `1`, or false, represented as `0`),
    * naturals: `n8`, `n16`, `n32`, `n64`, (naturals are *non-negative* integers which fit in a *signed* integer of the given bit size, e.g., `n32` can contain any number from `0` to `2^31 - 1`),
    * integers: `i8`, `i16`, `i32`, `i64`, (positive or negative integers which fit in a *signed* integer of the given bit size, excluding the most negative value, e.g., `i32` can contain any number from `-2^31 + 1` to `2^31 - 1`),
* aggregate types,
    * arrays, initialized in a `for` loop,
    * optionals, initialized in conditional statements (TODO),
* statically-typed variables,
* I/O instructions (`read` and `write`),
* invoking functions implemented by the problem solver (`call`),
    * with scalar or aggregate arguments,
    * optionally returning a scalar,
* range-based loops (`for`),
* generic loops (`loop`, `break`, `continue`) (TODO),
* conditionals and value-matching (`if`, `switch`) (TODO),
* arithmetic and boolean expressions (TODO),
* expression reuse with single-assignment variables (`let`) (TODO),
* checking input and output assumptions (`assume` and `assert`) (TODO).

Restrictions:

* *Variables must be assigned in exactly once place*.
* *Aggregate values can only be defined implicitly, by assigning one scalar at a time inside a control structure*. E.g., reading `A[i]: n32` in a `for i upto N` loop defines `A` as an array of `n32` of size `N`. Reading `X: n32` inside an `if` or `switch` defines `X` as an optional of `n32` (TODO).
* *All data variables must be named differently*. This simplifies referencing any given variable from outside the spec (say, in the documentation) and simplifies the generation of the skeleton code. NB: only the names of the variables have global scope, not the variables themeselves, so it is still an error to refer to a variable outside its definition scope.
* *Arguments of problem-solver functions must be simple variables, not expressions.* This makes sure that the parameter and the corresponding variable are the same object, and can be defined/documented only once. This restriction can be easily overcome by using a `let` statement before the `call` (TODO).
* *Reusable functions and/or recursion are not supported*. This allows mapping each lexical location in the code to a specific state of the parsers, and ensures that any given variable has at most one value at any given time (i.e., there are no stack frames).

## Usage

### Linting I/O specification

```
    turingarena-iospec lint
        [--spec-file <spec-file>]
```

Parses and lints the I/O specification in `<spec-file>`.

### Generating code

```
    turingarena-iospec code
        [--spec-file <spec-file>]
        [--target <file>]
        [--kind skeleton|template]
        [--language <lang>]
```

Generates skeleton or template code for a given language.

### Validating input and output files/streams (TODO)

```
    turingarena-iospec run
        [--spec-file <spec-file>]
        [--input-source <input-file-or-pipe>]
        [--output-source <output-file-or-pipe>]
        [--input-target <input-file-or-pipe>]
        [--output-target <output-file-or-pipe>]
        [--ignore-assumptions]
        [--ignore-assertions]
```

Parses and checks input, and optionally output, files or streams, according to an I/O specification.
Issues are reported on stderr.
If desired, generate the canonicalized for of the input or the output.

## Implementation design

Turingarena IOspec is implemented in Rust.

It parses the specification language exploting the parsing framework commonly used to implement Rust procedural macros.

## Example of generated code

An example of generated code for a probjem asking to find a cycle in a graph, already encoded in the input as adjacency lists.

Spec file:

```
read N: n32; // number of nodes

for u upto N {
    read D[u]: n32; // degree of u
    for i upto D[u] {
        read A[u][i]: n32; // adjacency list
    }
}

call find_cycle(N, D, A) -> L: n32; // length of cycle

write L;

for i upto L {
    call get_cycle_node(i) -> u: n32; // i-th node in the cycle
    write u;
}
```

Generated C++ code:

```c++
#include <cstdio>
#include <cstdint>

int32_t find_cycle(int32_t N, int32_t* D, int32_t** A);

int32_t get_cycle_node(int32_t i);

int main() {
    int32_t N;
    scanf("%d", &N);

    int32_t* D = new int32_t[N];
    int32_t** A = new int32_t*[N];
    for(int32_t u = 0; u < N; u++) {
        scanf("%d", &D[u]);

        A[u] = new int32_t[D[u]];
        for(int i = 0; i < D[u]; i++) {
            scanf("%d", &A[u][i]);
        }
    }

    int32_t L = find_cycle(N, D, A);

    printf("%d\n", L);

    for(int32_t i = 0; i < L; i++) {
        int32_t u = get_cycle_node(i);

        printf("%d\n", u);
    }
}
```