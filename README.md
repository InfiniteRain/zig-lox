# zig-lox

This is a bytecode runtime for Lox made in Zig. This mostly conforms to
the language outlined in the [book](https://craftinginterpreters.com/the-lox-language.html).
The implementation passes all the tests provided in the [repository for the
original book](https://github.com/munificent/craftinginterpreters). This
implementation also has some additional features:

1. const variables, prefixed with `const`,
2. bytecode support for 2^24 locals per scope (compilation disabled to make tests pass),
3. switch statement (identical to C, but doesn't require `break` keyword),
4. supports `continue` statemnt,
5. native functions have arity checking,
6. native functions can return a runtime error,
7. `in` keyword to check if a property exists on an instance `"property" in instance`,
8. `setField` native function to set an arbitrary string field on an instance,
9. `getField` native function to get an arbitrary string field on an instance,
10. `deleteField` native function to delete a field from an instance,
11. `gc` native function to force a garbage collection cycle to run.

This implementation doesn't include the NaN boxing optimization, as I didn't
reuse the macro approach outlined in the book. In order to implement NaN boxing,
I'm required to refactor some value-handling code, which could take some time.
I already got all I wanted from the book, so I decided not to invest any more
time into this.

# Usage

The runtime could be installed and used in the following way:

1. [install zig](https://ziglang.org/learn/getting-started/#installing-zig),
2. clone the repository: `git@github.com:InfiniteRain/zig-lox.git`,
3. cd into the cloned repository: `cd zig-lox`,
4. use the runtime in one of the following ways:
    - via REPL: `zig build run`,
    - via running a file: `zig build run -- my_lox_program.lox`.

Zig build accepts the following flags:

- `-Dprint-code` - prints the generated VM instructions,
- `-Dtrace-execution` - prints the contents of the stack between each instruction,
- `-Dstress-gc` - forces GC to run before every new allocation,
- `-Dlog-gc` - enables GC logging.

# License

MIT
