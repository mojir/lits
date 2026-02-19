# Lits — Marketing Content

## Small (one sentence)

Lits is a lightweight, sandboxed functional programming language that embeds directly in JavaScript applications — giving you safe, runtime-programmable logic with zero side effects.

A tiny, pure functional language that drops into any JavaScript runtime — giving users the power to script without the power to break things.

Safe enough for user input, powerful enough for real logic — Lits is a sandboxed functional language built to embed in JavaScript.

## Medium (~75 words)

Lits is a lightweight functional programming language designed to embed directly in JavaScript applications. It runs in a secure sandbox with no access to the file system, network, or globals — making it safe for user-supplied logic, custom formulas, and dynamic business rules. With immutable data, first-class functions, and a rich standard library, Lits brings the power of functional programming to any JS runtime without compromising security.

## Large (~250 words)

Lits is a pure functional programming language built to live inside your JavaScript applications. It provides a secure, sandboxed execution environment where user-supplied code can run without risk — no file system access, no network calls, no mutation of host application state.

This makes Lits ideal for scenarios where you need runtime programmability without sacrificing safety: custom business rules, dynamic formulas, scriptable workflows, plugin systems, and configuration logic that goes beyond what static data can express.

The language is expression-oriented — everything returns a value, from simple arithmetic to complex control flow. There are no statements, no `void`, no side effects. Data is immutable by default, and functions are first-class values that can be composed, passed as arguments, and returned from other functions.

Lits ships with a comprehensive standard library covering math, string manipulation, regular expressions, collection operations, and more. Domain-specific namespaces extend the language with vector math, linear algebra, matrix operations, and number theory — all opt-in to keep the core lightweight.

The syntax uses keyword-delimited blocks (`if...then...else...end`, `do...end`) and algebraic notation, making it immediately readable. Despite its simplicity, Lits supports closures, destructuring, higher-order functions, recursion with `loop`/`recur`, and pattern-matching-style constructs like `cond` and `switch`.

Integration is straightforward: create a `Lits` instance, call `run()` with a string of code, and get a result back. The entire runtime is a pure JavaScript library with zero dependencies, running anywhere JavaScript runs — browsers, Node.js, or edge runtimes.
