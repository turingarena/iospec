# Turingarena IOspec

A language to specify input/output format, and tools to automate I/O validation, parser generation, and more.

**NB**: Turingarena IOspec is work-in-progress. This README describes what it will do when complete.

## What is I/O

When defining a programming problem based on textual input/output, one needs to define a contract with the problem solver, where the following are specified:

* format of the input,
* assumption on the input which are guaranteed to hold,
* format of the output,
* assumption on the output which are required to hold for the output to be accepted.

In many [IOI](https://ioinformatics.org/)-like problems, the format follows some conventions. For example, often the input is line-based, where each line contains space-separated integers.

## What is Turingarena IOspec

Turingarena IOspec allows to specify formally the format of input and output, and some of the assumptions on them, as long as some very common conventions are followed (usual in IOI-like problems).

Once the format is specifies, the following tasks can be automated:

* validating input files,
* validating output files, given their respective input,
* validating input and output streams on-the-fly, for interactive problems,
* generating skeleton code which parses the input and formats the output, providing an abstraction on the I/O (in many different programming languages),
* generating template code, which, once filled by the problem solved, and combined with the skeleton, allows to have a working problem without the need for the problem solver to parse the input and format the output (in many different programming languages).
* generating documentation which describes the input/output format, and its assumptions.

## The specification language

In IOspec, the format is described in a language *similar to a programming language*, which defines, *from the perspective of the problem solver*:

* how the input should be parsed,
* when solution code needs to be executed to compute the output,
* how the ouput should be formatted,
* how and when assumption should be checked.

An example is worth a thousand words. Here is the I/O specification for the problem of finding a cycle in a graph encoded as an edge list.

```
read N:n32, M:n32;

assume 2 <= N && N <= 100_000;
assume 1 <= M && M <= 1_000_000;

for i to M {
    read A[i]:n32, B[i]:n32;

    assume 0 <= A[i] && A[i] < B[i] && B[i] < N;
}

call find_cycle(N, M, A, B) -> cycle_len:n32;
assert 2 <= cycle_len && cycle_len <= N;

write cycle_len;

for i to cycle_len {
    call get_cycle_node(i) -> u:n32;
    assert 0 <= u && u < N;

    write u;
}
```

The language supports many features common of programming languages, but it also has many restrictions which allow it to be used to automate some tasks (which otherwise would be harder or impossible), and at the same time making it more compact.

Features:

* statically-typed variables,
* I/O instructions (`read` and `write`),
* invoking functions implementedby the problem solver (`call`),
* range-based loops (`for`),
* generic loops (`loop`),
* conditionals (`if`),
* arithmetic and boolean expressions,
* constant variables for expression reuse (`let`),
* checking assumptions (`assume` and `assert`).

Restrictions:

* *Variables must be set exactly once each time they are in-scope*.
* *Reusable functions and/or recursion are not supported*. This allows mapping each lexical location in the code to a specific state of the parsers, and asserts that any given variable has at most one value at any given time.
* *All identifiers have global scope, and must be named differently*. This simplifies referencing any given variable from outside the spec (say, in the documentation) and simplifies the generation of the skeleton code. NB: only identifiers have global scope, not variables, so it is still an error to refer to a variable outside its definition scope.
* *Arguments of problem-solver functions must be simple variables, not expressions.* This makes sure that the parameter and the corresponding variable are the same object, and can be defined/documented only once.

## Usage

### Linting I/O specification

```
    turingarena-iospec lint <spec-file>
```

Parses and lints the I/O specification in `<spec-file>`.

### Validating input and output files/streams

```
    turingarena-iospec validate <spec-file> <input-file-or-pipe> [<output-file-or-pipe>]
        [--fail-on-input-error] [--ignore-assumptions]
        [--fail-on-output-error] [--ignore-assertions]
        [--canonicalize-input] [--canonicalize-input-to-file <dest-file-or-pipe>]
        [--canonicalize-output] [--canonicalize-output-to-file <dest-file-or-pipe>]
        [--explain] [--explain-to-file <dest-file-or-pipe>]
```

Parses and checks input, and optionally output, files or streams, according to an I/O specification.
Issues are reported on stderr.
If desired, canonicalize the input or the output, or explain how the streams were parsed (showing values of the variables, and so on).

### Generating code

```
    turingarena-iospec codegen <spec-file>
        [--target skeleton|template]
        [--language <lang>]
        [--output-file <file>]
```

Generates skeleton or template code for a given language.

## Implementation design

Turingarena IOspec is implemented in Rust.

It parses the specification language exploting the parsing framework commonly used to implement Rust procedural macros.
